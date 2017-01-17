#include "docopt_fish.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <algorithm>
#include <iostream>
#include <memory>
#include <numeric>
#include <set>
#include "docopt_fish_grammar.h"
#include "docopt_fish_types.h"

using std::shared_ptr;

OPEN_DOCOPT_IMPL

static const size_t npos = string_t::npos;

typedef std::vector<rstring_t> rstring_list_t;
typedef std::vector<error_t> error_list_t;
typedef std::vector<size_t> index_list_t;

// Class representing a metadata map. Keys are options and variables.
typedef base_metadata_t<rstring_t> rmetadata_t;
typedef std::map<rstring_t, rmetadata_t> metadata_map_t;

#pragma mark -
#pragma mark Matching
#pragma mark -

// The result of parsing argv
typedef std::map<rstring_t, argument_t> option_rmap_t;

// COW helper
// Wraps a shared_ptr and only provides const access to it,
// except for the mut() function
// This can never hold NULL, except after being moved from
template<typename T>
class cow_ref_t {
    shared_ptr<T> ptr;

public:
    cow_ref_t(const cow_ref_t &) = default;
    cow_ref_t(cow_ref_t &&rhs) = default;
    cow_ref_t &operator=(cow_ref_t &&rhs) = default;

    explicit cow_ref_t(T &&val) : ptr(std::make_shared<T>(val))
    {}

    T& mut() {
        // perform the copy-on-write if there is no unique owner
        if (! ptr.unique()) {
            ptr = std::make_shared<T>(*ptr);
        }
        return *ptr;
    }

    const T& operator*() const {
        return *ptr;
    }

    const T* operator->() const {
        return &**this;
    }
};

// Match state objects
// Match states represent the state of matching argv against our docopt usage tree
// These are passed around and may be copied, etc.
// Matching is very performance sensitive, so this uses a few optimization tricks
// Implicit copying and moving (via constructors, std::move, etc.) is not allowed
// Instead use copy() and move()
// Also, since states need to diverge but often share structure, we use shared_ptr
// extensively, and copy-on-write
struct match_state_t {
    friend struct match_context_t;

   private:
    // Copying must be explicit via copy()
    // Or use move()
    match_state_t(const match_state_t &) = default;
    match_state_t &operator=(const match_state_t &) = delete;

public:
    // Map from option names to arguments
    cow_ref_t<option_rmap_t> argument_values;

    // Bitset of options we've consumed
    cow_ref_t<std::vector<bool>> consumed_options;

    // Next positional to dequeue
    size_t next_positional_index;

    // Suggestions generated for this state
    std::set<rstring_t> suggested_next_arguments;

    // Whether this match has fully consumed all positionals and options
    bool fully_consumed;

    explicit match_state_t(size_t option_count)
        : argument_values(option_rmap_t()),
          consumed_options(std::vector<bool>(option_count, false)),
          next_positional_index(0),
          fully_consumed(false) {}

    // Default move assignment and constructors
    match_state_t &operator=(match_state_t &&) = default;
    match_state_t(match_state_t &&) = default;

    // Explicit copying only
    match_state_t copy() const {
        return *this;
    }

    // Convenience over std::move()
    match_state_t &&move() {
        return std::move(*this);
    }

    // Returns the "progress" of a state. This is a sum of the number of
    // positionals and
    // arguments consumed, plus the number of suggestions. This is not directly
    // comparable
    // (two states may have identical progress values but be different) but it
    // does capture
    // the relationship between a state and another state derived from it, i.e. if
    // the
    // progress does not change then the child state is identical to its parent.
    size_t progress() const {
        // Add in positionals
        size_t result = this->next_positional_index;

        // Add in arguments by counting set bits in the bitmap
        result += std::accumulate(this->consumed_options->cbegin(),
                                  this->consumed_options->cend(),
                                  0);

        // Add in number of suggestions
        result += suggested_next_arguments.size();

        return result;
    }
};

typedef std::vector<match_state_t> match_state_list_t;

struct match_context_t {
   private:

    // No copying
    match_context_t(const match_context_t &) = delete;
    void operator=(const match_context_t &) = delete;

    // Returns true if the state has consumed all positionals and options
    bool has_consumed_everything(const match_state_t &state) const {
        if (has_more_positionals(state)) {
            // Unconsumed positional
            return false;
        }

        // Now return whether all of our consumed options are true
        const std::vector<bool> &ops = *state.consumed_options;
        return std::all_of(ops.begin(), ops.end(), [](bool v) { return v; });
    }

   public:
    const parse_flags_t flags;

    // Note: these are stored references.
    // Match context objects are expected to be transient and stack-allocated.
    const option_list_t &shortcut_options;
    const arg_classification_t &aclass;
    const string_list_t &argv;

    bool has_more_positionals(const match_state_t &state) const {
        assert(state.next_positional_index <= this->aclass.positionals.size());
        return state.next_positional_index < this->aclass.positionals.size();
    }

    // Returns the indexes in argv of the arguments that were unused
    index_list_t unused_arguments(const match_state_t *state) const {
        // To find the unused arguments, we walk over the used arguments and take
        // what's left
        // Arguments may be unused for any of three reasons:
        // 1. It is an unconsumed positional
        // 2. It is an option that we found in the tree, but was not matched during
        //    tree descent
        // 3. It is an option that was not found in the tree at all
        // Make a vector the same size as argv.
        // As we walk over positionals and options, we will mark the corresponding
        // index as used.
        // At the end, the unset bits are the unused arguments
        std::vector<bool> used_argv_indexes(this->argv.size(), false);
        
        auto mark_used = [&](size_t idx) {
            if (idx != npos)
                used_argv_indexes.at(idx) = true;
        };
        auto mark_unused = [&](size_t idx) {
            if (idx != npos)
                used_argv_indexes.at(idx) = false;
        };
        
        // consumed_options is a vector parallel to resolved_options,
        // which tracks whether each resolved_option was used
        const std::vector<bool> &copts = *state->consumed_options;

        // Walk over used positionals.
        // next_positional_index is the first unused one
        for (size_t i = 0; i < state->next_positional_index; i++) {
            mark_used(this->aclass.positionals.at(i).idx_in_argv);
        }

        // Walk over options matched during tree descent.
        // We should have one bit per option
        assert(copts.size() == this->aclass.resolved_options.size());
        for (size_t i = 0; i < copts.size(); i++) {
            if (copts.at(i)) {
                // This option was used. The name index is definitely used. The value
                // index is also used, if it's not npos
                // (note that it may be the same as the name index)
                const resolved_option_t &opt = this->aclass.resolved_options.at(i);
                mark_used(opt.name_idx_in_argv);
                mark_used(opt.value_idx_in_argv);
            }
        }

        // Walk over options NOT matched during tree descent and clear their bits.
        // An argument may be both matched and unmatched, i.e. if "-vv" is parsed
        // into two short options.
        // In that case, we want to mark it as unmatched.
        for (size_t i = 0; i < copts.size(); i++) {
            if (!copts.at(i)) {
                const resolved_option_t &opt = this->aclass.resolved_options.at(i);
                mark_unused(opt.name_idx_in_argv);
            }
        }

        // Don't report the double-dash argument as unused
        mark_used(this->aclass.double_dash_idx);

        // Extract the unused indexes from the bitmap of used arguments
        index_list_t unused_argv_idxs;
        for (size_t i = 0; i < used_argv_indexes.size(); i++) {
            if (!used_argv_indexes.at(i)) {
                unused_argv_idxs.push_back(i);
            }
        }
        return unused_argv_idxs;
    }

    const positional_argument_t &next_positional(match_state_t *state) const {
        assert(state->next_positional_index < this->aclass.positionals.size());
        return this->aclass.positionals.at(state->next_positional_index);
    }

    const positional_argument_t &acquire_next_positional(match_state_t *state) const {
        assert(state->next_positional_index < this->aclass.positionals.size());
        return this->aclass.positionals.at(state->next_positional_index++);
    }

    match_context_t(parse_flags_t f, const option_list_t &shortcut_opts,
                    const arg_classification_t &classification,
                    const string_list_t &av)
        : flags(f),
          shortcut_options(shortcut_opts),
          aclass(classification),
          argv(av) {}

    // If we want to stop a search and this state has consumed everything, stop
    // the search
    void try_mark_fully_consumed(match_state_t *state) const {
        if ((this->flags & flag_stop_after_consuming_everything) &&
            this->has_consumed_everything(*state)) {
            state->fully_consumed = true;
        }
    }
};

static void match(const vector<usage_t> &usages, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states);
static void match(const usage_t &node, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states);
static void match(const expression_list_t &node, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states);
static void match(const alternation_list_t &node, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states);
static void match(const expression_t &node, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states);
static void match(const simple_clause_t &node, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states);
static void match(const option_clause_t &node, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states);
static void match(const fixed_clause_t &node, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states);
static void match(const variable_clause_t &node, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states);

static bool match_options(const option_list_t &options_in_doc, match_state_t *state,
                          const match_context_t &ctx);

// Given a node and list of states, match the node for each state in the list
// Append the results to the resulting_states
// If require_progress is true, then discard new states that don't make progress
template <typename T>
static void match_list(const T &node, match_state_list_t &&incoming_state_list, const match_context_t &ctx,
                       match_state_list_t *resulting_states, bool require_progress = false) {
    for (match_state_t &state : incoming_state_list) {
        // If we require that this makes progress, then get the current progress so
        // we can compare
        size_t init_progress = npos;
        size_t init_size = -1;
        if (require_progress) {
            init_progress = state.progress();
            init_size = resulting_states->size();
        }

        match(node, state.move(), ctx, resulting_states);

        if (require_progress) {
            // Keep only those results that have increased in progress. States after
            // init_size are new.
            size_t idx = resulting_states->size();
            assert(idx >= init_size);
            while (idx-- > init_size) {  // last processed idx will be init_size
                size_t new_progress = resulting_states->at(idx).progress();
                assert(new_progress >= init_progress);  // should never go backwards
                if (new_progress == init_progress) {
                    // No progress was made, toss this state
                    resulting_states->erase(resulting_states->begin() + idx);
                }
            }
        }
    }
}

static void match(const vector<usage_t> &usages, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states) {
    // Elide the copy in the last one
    size_t count = usages.size();
    if (count == 0) {
        return;
    }

    bool fully_consumed = false;
    for (size_t i = 0; i + 1 < count && !fully_consumed; i++) {
        match(usages.at(i), state.copy(), ctx, resulting_states);

        if (ctx.flags & flag_stop_after_consuming_everything) {
            for (const match_state_t &ms : *resulting_states) {
                if (ms.fully_consumed) {
                    fully_consumed = true;
                    break;
                }
            }
        }
    }
    if (!fully_consumed) {
        match(usages.at(count - 1), state.move(), ctx, resulting_states);
    }
}

static void match(const usage_t &node, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states) {
    if (!ctx.has_more_positionals(state)) {
        // todo: error handling
        return;
    }

    if (node.prog_name.empty()) {
        // This is the final terminating usage. It does not match anything.
        return;
    }

    // Program name, then match against our contents
    ctx.acquire_next_positional(&state);
    match(node.alternation_list, state.move(), ctx, resulting_states);
}

static void match(const expression_list_t &node, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states) {
    size_t count = node.expressions.size();
    if (count == 0) {
        // Empty expression list, append the state
        resulting_states->push_back(state.move());
    } else if (count == 1) {
        // Common case of just one expression, trivial
        match(node.expressions.at(0), state.move(), ctx, resulting_states);
    } else {
        // First expression
        match_state_list_t intermed_state_list;
        match(node.expressions.at(0), state.move(), ctx, &intermed_state_list);
        // Middle expressions
        for (size_t i = 1; i + 1 < count; i++) {
            match_state_list_t new_states;
            match_list(node.expressions.at(i), std::move(intermed_state_list), ctx, &new_states);
            intermed_state_list = std::move(new_states);
        }
        // Last expression
        match_list(node.expressions.at(count - 1), std::move(intermed_state_list), ctx, resulting_states);
    }
}

static void match(const alternation_list_t &node, match_state_t state,
                  const match_context_t &ctx, match_state_list_t *resulting_states) {
    size_t count = node.alternations.size();
    if (count == 0) {
        return;
    }
    for (size_t i = 0; i + 1 < count; i++) {
        match(node.alternations.at(i), state.copy(), ctx, resulting_states);
    }
    match(node.alternations.at(count - 1), state.move(), ctx, resulting_states);
}

// Ellipsis helper
// Given a node that has ellipsis, the set of resulting states, and an initial
// prior state count, match the node repeatedly until we stop making progress
template <typename Node>
static void repeat_matching(const Node &node, size_t init_prior_state_count, const match_context_t &ctx,
                            match_state_list_t *resulting_states) {
    assert(resulting_states->size() >= init_prior_state_count);
    // Get the number of existing states
    // Keep going until we stop getting new states
    size_t prior_state_count = init_prior_state_count;
    while (prior_state_count < resulting_states->size()) {
        // Get a vector of intermediate states
        match_state_list_t intermediate_states;
        intermediate_states.reserve(resulting_states->size() - prior_state_count);
        for (size_t i = prior_state_count; i < resulting_states->size(); i++) {
            intermediate_states.push_back(resulting_states->at(i).copy());
        }
        prior_state_count = resulting_states->size();
        match_list(node, std::move(intermediate_states), ctx, resulting_states, true /* require progress */);
    }
}

static void match(const expression_t &node, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states) {
    // Check to see if we have ellipsis. If so, we keep going as long as we can.
    bool has_ellipsis = node.opt_ellipsis.present;

    switch (node.production) {
        case 0: {
            // This is a simple clause which may have ellipsis, like foo...
            // If we have ellipsis, we match one time, and then construct a sequence of
            // 'intermediate state lists'.
            //  An intermediate state represents the result of matching N times. Each time
            //  we construct a new intermediate state, we append (copy) all of its states into the
            //  result; thus we may match one time, two times, three times...
            //  We stop when we get no more matches, which usually happens when we run out
            //  of positionals.
            assert(node.simple_clause.get() != nullptr);
            size_t prior_state_count = resulting_states->size();
            match(*node.simple_clause, state.move(), ctx, resulting_states);
            // Now we know that all states starting at state_count_before are newly added.
            // If we have ellipsis, go until we stop getting new states.
            if (has_ellipsis) {
                repeat_matching(*node.simple_clause, prior_state_count, ctx, resulting_states);
            }
            break;
        }

        case 1: {
            // This is a parenthesized clause which may have ellipsis, like (foo)...
            // Same algorithm as the simple clause above.
            size_t prior_state_count = resulting_states->size();
            assert(node.alternation_list.get() != nullptr);
            match(*node.alternation_list, state.move(), ctx, resulting_states);
            if (has_ellipsis) {
                repeat_matching(*node.alternation_list, prior_state_count, ctx, resulting_states);
            }
            break;
        }

        case 2: {
            // This is a square-bracketed clause which may have ellipsis, like [foo]...
            // Same algorithm as the simple clause above, except that we also append the
            // initial state as a not-taken branch.
            assert(node.alternation_list.get() != nullptr);
            resulting_states->push_back(state.copy());  // append the not-taken-branch
            size_t prior_state_count = resulting_states->size();
            match(*node.alternation_list, std::move(state), ctx, resulting_states);
            if (has_ellipsis) {
                repeat_matching(*node.alternation_list, prior_state_count, ctx, resulting_states);
            }
            break;
        }

        case 3: {
            // This is the [options] clause. It does not have ellipsis.
            assert(node.options_shortcut.present);
            if (!match_options(ctx.shortcut_options, &state, ctx)) {
                // No match, but matches are not required
                if (ctx.flags & flag_generate_suggestions) {
                    for (const option_t &opt : ctx.shortcut_options) {
                        state.suggested_next_arguments.insert(opt.best_name());
                    }
                }
            }
            resulting_states->push_back(state.move());
            break;
        }

        default:
            assert(0 && "unknown production");
    }
}

// Match the options in the options list, updating the state
// This returns true if we match at least one
static bool match_options(const option_list_t &options_in_doc, match_state_t *state,
                          const match_context_t &ctx) {
    bool successful_match = false;
    bool made_suggestion = false;

    // Collect potential suggestions in here. We squelch them if we find that a
    // later matched option
    // has the same corresponding long name; we need to remove those from the
    // suggestions
    option_list_t potential_suggestions;

    for (const option_t &opt_in_doc : options_in_doc) {
        // Find the matching option from the resolved option list (i.e. argv)
        size_t resolved_opt_idx = npos;
        for (size_t i = 0; i < ctx.aclass.resolved_options.size(); i++) {
            // Skip ones that have already been consumed
            if (! state->consumed_options->at(i)) {
                // See if the option from argv has the same key range as the option in
                // the document
                if (ctx.aclass.resolved_options.at(i).option.has_same_name(opt_in_doc)) {
                    resolved_opt_idx = i;
                    break;
                }
            }
        }

        if (resolved_opt_idx != npos) {
            // We found a matching option in argv. Set it in the argument_values for
            // this state and mark its index as used
            // We have two things to set:
            //  - The option name, like -foo
            //  - The option's argument value (if any)
            const resolved_option_t &resolved_opt = ctx.aclass.resolved_options.at(resolved_opt_idx);
            const rstring_t &name = opt_in_doc.best_name();

            // Update the option value, creating it if necessary
            state->argument_values.mut()[name].count += 1;

            // Update the option argument vlaue
            if (opt_in_doc.has_value() && resolved_opt.value_idx_in_argv != npos) {
                const rstring_t &variable_name = opt_in_doc.value;

                const rstring_t &value = resolved_opt.value_in_arg;
                state->argument_values.mut()[variable_name].values.push_back(value.std_string());
            }

            successful_match = true;
            state->consumed_options.mut()[resolved_opt_idx] = true;
        } else {
            // This was an option that was not specified in argv
            // It can be a suggestion
            if (ctx.flags & flag_generate_suggestions) {
                potential_suggestions.push_back(opt_in_doc);
            }
        }
    }

    // Now go through and handle potential suggestions
    if (ctx.flags & flag_generate_suggestions) {
        for (const option_t &suggestion : potential_suggestions) {
            size_t type_idx = option_t::NAME_TYPE_COUNT;
            while (type_idx--) {
                option_t::name_type_t type = static_cast<option_t::name_type_t>(type_idx);
                if (suggestion.has_type(type)) {
                    state->suggested_next_arguments.insert(suggestion.names[type]);
                }
            }
            made_suggestion = true;
        }
    }

    bool matched_something = successful_match || made_suggestion;
    if (matched_something) {
        ctx.try_mark_fully_consumed(state);
    }
    return matched_something;
}

static void match(const simple_clause_t &node, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states) {
    if (node.option.get()) {
        match(*node.option, state.move(), ctx, resulting_states);
    } else if (node.fixed.get()) {
        match(*node.fixed, state.move(), ctx, resulting_states);
    } else if (node.variable.get()) {
        match(*node.variable, state.move(), ctx, resulting_states);
    } else {
        assert(0 && "Bug in docopt parser: No children of simple_clause.");
    }
}

static void match(const option_clause_t &node, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states) {
    // Matching an option like --foo
    const option_list_t options_in_doc(1, node.option);
    bool matched = match_options(options_in_doc, &state, ctx);
    if (matched || (ctx.flags & flag_match_allow_incomplete)) {
        resulting_states->push_back(state.move());
    }
}

static void match(const fixed_clause_t &node, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states) {
    // Fixed argument
    // Compare the next positional to this static argument
    if (ctx.has_more_positionals(state)) {
        const positional_argument_t &positional = ctx.next_positional(&state);
        rstring_t name = rstring_t(ctx.argv.at(positional.idx_in_argv));
        if (node.word == name) {
            // The static argument matches
            state.argument_values.mut()[name].count += 1;
            ctx.acquire_next_positional(&state);
            ctx.try_mark_fully_consumed(&state);
            resulting_states->push_back(state.move());
        }
    } else {
        // No more positionals. Maybe suggest one.
        if (ctx.flags & flag_generate_suggestions) {
            state.suggested_next_arguments.insert(node.word);
        }
        // Append the state if we are allowing incomplete
        if (ctx.flags & flag_match_allow_incomplete) {
            resulting_states->push_back(state.move());
        }
    }
}

static void match(const variable_clause_t &node, match_state_t state, const match_context_t &ctx,
                  match_state_list_t *resulting_states) {
    // Variable argument
    const rstring_t &name = node.word;
    if (ctx.has_more_positionals(state)) {
        // Note we retain the brackets <> in the variable name
        // Note also that 'arg' points into an entry in the state argument value map
        const positional_argument_t &positional = ctx.acquire_next_positional(&state);
        const string_t &positional_value = ctx.argv.at(positional.idx_in_argv);
        option_rmap_t &args = state.argument_values.mut();
        args[name].values.push_back(positional_value);
        ctx.try_mark_fully_consumed(&state);
        resulting_states->push_back(state.move());
    } else {
        // No more positionals. Suggest one.
        if (ctx.flags & flag_generate_suggestions) {
            state.suggested_next_arguments.insert(name);
        }
        if (ctx.flags & flag_match_allow_incomplete) {
            resulting_states->push_back(state.move());
        }
    }
}

static inline std::basic_ostream<string_t::value_type> &errstream() {
#if DOCOPT_USE_WCHAR
    return std::wcerr;
#else
    return std::cerr;
#endif
}

match_results_t match_usages(const std::vector<usage_t> &usages,
                             parse_flags_t flags,
                             const option_list_t &shortcut_options,
                             const arg_classification_t &aclass,
                             const string_list_t &argv) {
    
    match_context_t ctx(flags, shortcut_options, aclass, argv);
    size_t opt_count = ctx.aclass.resolved_options.size();
    match_state_list_t states;
    match(usages, match_state_t(opt_count), ctx, &states);
    
    // Illustration of some logging to help debug matching
    const bool log_stuff = false;
    if (log_stuff) {
        errstream() << "Matched " << states.size() << " way(s)\n";
        for (size_t i = 0; i < states.size(); i++) {
            const match_state_t &state = states.at(i);
            bool is_incomplete = !ctx.unused_arguments(&state).empty();
            errstream() << "Result " << i << (is_incomplete ? " (INCOMPLETE)" : "") << ":\n";
            for (const auto &kv : *state.argument_values) {
                const rstring_t &name = kv.first;
                const argument_t &arg = kv.second;
                errstream() << "\t" << name.std_string() << ": ";
                for (size_t j = 0; j < arg.values.size(); j++) {
                    if (j > 0) {
                        errstream() << ", ";
                    }
                    errstream() << arg.values.at(j);
                }
                errstream() << '\n';
            }
        }
    }
    
    match_results_t r;
    if (states.empty()) {
        // We had no states
        // Every arg is unused
        for (size_t i = 0; i < argv.size(); i++) {
            r.unused_args.push_back(i);
        }
    } else {
        // We got at least one state
        // Find the ones with the fewest unused arguments
        size_t fewest_unused_args = SIZE_MAX;
        for (match_state_t &st: states) {
            index_list_t unused_args = ctx.unused_arguments(&st);
            if (unused_args.size() < fewest_unused_args) {
                // Fewer unused args
                // Replace the suggestions
                fewest_unused_args = unused_args.size();
                r.option_map = std::move(st.argument_values.mut());
                r.unused_args = std::move(unused_args);
                r.suggestions = std::move(st.suggested_next_arguments);
            } else if (unused_args.size() == fewest_unused_args) {
                // Same number of unused args
                // Incorporate these suggestions too
                r.suggestions.insert(st.suggested_next_arguments.cbegin(),
                                     st.suggested_next_arguments.cend());
            }
        }
    }
    
    return r;
}


// close the namespace
CLOSE_DOCOPT_IMPL
