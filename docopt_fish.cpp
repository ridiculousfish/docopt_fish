#include "docopt_fish.h"
#include <memory>
#include <assert.h>
#include <cstring>
#include <cstdint>
#include <iostream>

#define UNUSED __attribute__((unused))


/* Hide open and close brackets to avoid an annoying leading indent inside our class */
#define OPEN_DOCOPT_IMPL {
#define CLOSE_DOCOPT_IMPL }

namespace docopt_fish
OPEN_DOCOPT_IMPL

using std::auto_ptr;

static const size_t npos = (size_t)(-1);

typedef std::vector<size_t> index_list_t;

/* Overloads */
UNUSED
static void assign_narrow_string_to_string(const char *s, std::string *result) {
    *result = s;
}

UNUSED
static void assign_narrow_string_to_string(const char *s, std::wstring *result) {
    size_t len = std::strlen(s);
    for (size_t i=0; i < len; i++) {
        char c = s[i];
        assert(c <= 127); //ASCII only
        result->push_back(wchar_t(c));
    }
}

UNUSED
static const std::wstring &widen(const std::wstring &t) {
    return t;
}

UNUSED
static std::wstring widen(const std::string &t) {
    std::wstring result;
    result.insert(result.begin(), t.begin(), t.end());
    return result;
}


// Need a wstring implementation of this
static size_t find_case_insensitive(const std::string &haystack, const char *needle, size_t haystack_start) {
    assert(haystack_start < haystack.size());
    const char *haystack_cstr = haystack.c_str();
    const char *found = strcasestr(haystack_cstr + haystack_start, needle);
    return found ? found - haystack_cstr : std::string::npos;
}

static size_t find_case_insensitive(const std::wstring &haystack, const char *needle, size_t haystack_start) {
    // Nasty implementation
    // The assumption here is that needle is always ASCII; thus it suffices to do an ugly tolower comparison
    assert(haystack_start < haystack.size());
    const size_t needle_len = strlen(needle);
    if (needle_len > haystack.size()) {
        // needle is longer than haystack, no possible match
        return std::wstring::npos;
    }

    const wchar_t *haystack_cstr = haystack.c_str();
    size_t search_end = haystack.size() - needle_len;
    
    for (size_t i=haystack_start; i < search_end; i++) {
        // See if we have a match at i
        size_t j;
        for (j = 0; j < needle_len; j++) {
            wchar_t wc = haystack_cstr[i + j];
            if (wc > 127 || tolower((char)wc) != tolower(needle[j])) {
                break;
            }
        }
        if (j == needle_len) {
            // Matched them all
            return i;
        }
    }
    return std::wstring::npos;
}

/* Class representing a range of a string */
struct range_t {
    size_t start;
    size_t length;
    range_t() : start(0), length(0) {}
    range_t(size_t s, size_t l) : start(s), length(l) {}

    size_t end() const {
        size_t result = start + length;
        assert(result >= start); //don't overflow
        return result;
    }

    bool empty() const {
        return length == 0;
    }

    bool operator==(const range_t &rhs) const {
        return this->start == rhs.start && this->length == rhs.length;
    }

    // Range comparison so these can be used as dictionary keys
    bool operator<(const range_t &rhs) const {
        if (this->start != rhs.start) {
            return this->start < rhs.start;
        }
        return this->length < rhs.length;
    }

    // Merges a range into this range. Empty ranges are discarded.
    void merge(const range_t &rhs) {
        if (this->empty()) {
            *this = rhs;
        } else if (! rhs.empty()) {
            this->start = std::min(this->start, rhs.start);
            this->length = std::max(this->end(), rhs.end()) - this->start;
        }
    }
};
typedef std::vector<range_t> range_list_t;

/* A token is just a range of some string, with a type */
struct token_t {
    range_t range;
    token_t(const range_t &r) : range(r) {}
    token_t() {}

    bool empty() const {
        return range.empty();
    }
};
typedef std::vector<token_t> token_list_t;

/* An option represents something like '--foo=bar' */
struct option_t {
    enum type_t {
        single_short, // '-f'
        single_long, // '-foo'
        double_long // '--foo'
    } type;
    
    enum separator_t {
        sep_space, // curl -o file
        sep_equals, // -std=c++98
        sep_none // -DNDEBUG. Must be a single_short option.
    };

    // name of the option, like '--foo'
    range_t name;

    // value of the option, i.e. variable name. Empty for no value.
    range_t value;
    
    // key name of the option, e.g. if we have both -v and --verbose, this will be 'verbose' for the -v option. Empty for none.
    range_t key_range;

    // Range of the description. Empty for none.
    range_t description_range;
    
    // Range of the default value. Empty for none.
    range_t default_value_range;

    // How we separate the name from the value. We may make this a bitmap some day.
    separator_t separator;

    option_t(const range_t &n, const range_t &v, size_t leading_dash_count, separator_t sep) : name(n), value(v), separator(sep) {
        // Set the type. If there is only one dash, we infer single_long and single_short by the length of the name
        if (leading_dash_count > 1) {
            this->type = option_t::double_long;
        } else if (n.length > 1) {
            this->type = option_t::single_long;
        } else {
            this->type = option_t::single_short;
        }
    }

    /* We want a value if we have a non-empty value range */
    bool has_value() const {
        return ! value.empty();
    }

    bool operator==(const option_t &rhs) const {
        return this->type == rhs.type &&
               this->separator == rhs.separator &&
               this->name == rhs.name &&
               this->value == rhs.value;
    }

    /* Helper function for dumping */
    template<typename string_t>
    string_t describe(const string_t &src) const {
        string_t result;
        string_t tmp;
        
        result.append(src, name.start, name.length);
        if (! value.empty()) {
            result.push_back(':');
            result.push_back(' ');
            result.append(src, value.start, value.length);
        }
        result.push_back(' ');
        
        char range[64];
        snprintf(range, sizeof range, "<%lu, %lu>", name.start, name.length);
        assign_narrow_string_to_string(range, &tmp);
        result.append(tmp);
        
        if (this->separator == sep_none) {
            assign_narrow_string_to_string(" (no sep)", &tmp);
            result.append(tmp);
        } else if (this->separator == sep_none) {
            assign_narrow_string_to_string(" (= sep)", &tmp);
            result.append(tmp);
        }

        
        return result;
    }
    
    // Modifies the string to the name of the option, by plucking it out of the source. Includes dashes.
    template<typename string_t>
    void assign_key_to_string(const string_t &src, string_t *result) const {
        // Everyone gets at least one dash; doubles get two
        result->clear();
        bool use_key_range = ! this->key_range.empty();
        result->push_back('-');
        if (use_key_range || this->type == double_long) {
            result->push_back('-');
        }
        const range_t &effective_range = use_key_range ? this->key_range : this->name;
        result->append(src, effective_range.start, effective_range.length);
    }
    
    template<typename string_t>
    string_t key_as_string(const string_t &src) const {
        string_t result;
        this->assign_key_to_string(src, &result);
        return result;
    }
};
typedef std::vector<option_t> option_list_t;


/* Wrapper template class that takes either a string or wstring as string_t */
template<typename string_t>
class docopt_impl OPEN_DOCOPT_IMPL

/* A character in string_t; likely either char or wchar_t */
typedef typename string_t::value_type char_t;

#pragma mark -
#pragma mark Scanning
#pragma mark -

static bool char_is_valid_in_parameter(char_t c) {
    const char *invalid = ".|<>,=()[] \t\n";
    const char *end = invalid + strlen(invalid);
    return std::find(invalid, end, c) == end;
}

static bool char_is_valid_in_variable(char_t c) {
    const char *invalid = ".|<>,=()[] \t\n";
    const char *end = invalid + strlen(invalid);
    return std::find(invalid, end, c) == end;
}

static bool char_is_valid_in_word(char_t c) {
    const char *invalid = ".|()[],= \t\n";
    const char *end = invalid + strlen(invalid);
    return std::find(invalid, end, c) == end;
}

static bool char_is_space_or_tab(char_t c) {
    return c == ' ' || c == '\t';
}

template<char T>
static bool it_equals(char_t c) { return c == T; }

template<typename T>
static range_t scan_1(const string_t &str, range_t *remaining, T func) {
    range_t result(remaining->start, 0);
    if (result.end() < remaining->end() && func(str.at(result.end()))) {
        result.length += 1;
        remaining->start += 1;
        remaining->length -= 1;
    }
    return result;
}

static range_t scan_1_char(const string_t &str, range_t *remaining, char_t c) {
    range_t result(remaining->start, 0);
    if (result.end() < remaining->end() && str.at(result.end()) == c) {
        result.length += 1;
        remaining->start += 1;
        remaining->length -= 1;
    }
    return result;
}

template<typename T>
static range_t scan_while(const string_t &str, range_t *remaining, T func) {
    range_t result(remaining->start, 0);
    while (result.end() < remaining->end() && func(str.at(result.end()))) {
        result.length += 1;
        remaining->start += 1;
        remaining->length -= 1;
    }
    return result;
}

#pragma mark -
#pragma mark Usage Grammar
#pragma mark -

/* Usage grammar:
 
 usage = <empty> |
         WORD usage |
         WORD alternation_list usage
 
 alternation_list = expression_list or_continuation
 
 or_continuation = <empty> |
                   VERT_BAR alternation_list
 
 expression_list = expression opt_expression_list
 
 opt_expression_list = <empty> |
                       expression_list
 
 expression = simple_clause opt_ellipsis |
              OPEN_PAREN alternation_list CLOSE_PAREN opt_ellipsis |
              OPEN_SQUARE alternation_list CLOSE_SQUARE opt_ellipsis
              options_shortcut
 
 simple_clause = WORD
 
 opt_ellipsis = <empty> |
 ELLIPSIS
 
 options_shortcut = OPEN_SQUARE WORD CLOSE_SQUARE
 
 */

struct alternation_list;
struct expression_list_t;
struct opt_expression_list_t;
struct expression_t;
struct or_continuation_t;
struct simple_clause_t;
struct opt_ellipsis_t;

/* Context passed around in our recursive descent parser */
struct parse_context_t {
     // Note unowned pointer reference. A parse context is stack allocated and transient.
    const string_t *source;
    range_t remaining_range;
    
    parse_context_t(const string_t &src, const range_t &usage_range) : source(&src), remaining_range(usage_range)
    {}
    
    /* Consume leading whitespace, except newlines, which are meaningful and therefore tokens in their own right */
    void consume_leading_whitespace() {
        scan_while(*source, &remaining_range, char_is_space_or_tab);
    }
    
    /* Returns true if there are no more next tokens */
    bool is_at_end() const {
        return remaining_range.length == 0;
    }
    
    bool scan(char_t c, token_t *tok = NULL) {
        this->consume_leading_whitespace();
        token_t storage;
        storage.range = scan_1_char(*source, &remaining_range, c);
        if (tok) {
            *tok = storage;
        }
        return ! storage.range.empty();
    }
    
    bool scan(const char *c, token_t *tok) {
        this->consume_leading_whitespace();
        bool success = true;
        token_t string_tok;
        const range_t saved_remaining = this->remaining_range;
        for (size_t i=0; c[i] != '\0'; i++) {
            token_t char_tok;
            if (! this->scan(c[i], &char_tok)) {
                success = false;
                break;
            }
            string_tok.range.merge(char_tok.range);
        }
        
        // If we did not scan all the characters, restore our initial state
        if (! success) {
            this->remaining_range = saved_remaining;
            string_tok.range = range_t(0, 0);
        }
        if (tok) {
            *tok = string_tok;
        }
        return success;
    }
    
    bool scan_word(token_t *tok) {
        this->consume_leading_whitespace();
        tok->range = scan_while(*source, &remaining_range, char_is_valid_in_word);
        return ! tok->range.empty();
    }
    
    token_t peek_word() {
        this->consume_leading_whitespace();
        token_t tok;
        range_t local_range = remaining_range;
        tok.range = scan_while(*source, &local_range, char_is_valid_in_word);
        return tok;
    }
};


/* Helpers to parse when the productions are fixed. */
template<typename PARENT, typename CHILD>
static PARENT *parse_1(parse_context_t *ctx) {
    PARENT *result = NULL;
    auto_ptr<CHILD> child(CHILD::parse(ctx));
    if (child.get() != NULL) {
        result = new PARENT(child);
    }
    return result;
}

template<typename PARENT, typename CHILD1, typename CHILD2>
static PARENT *parse_2(parse_context_t *ctx) {
    PARENT *result = NULL;
    auto_ptr<CHILD1> child1(CHILD1::parse(ctx));
    if (child1.get()) {
        auto_ptr<CHILD2> child2(CHILD2::parse(ctx));
        if (child2.get()) {
            result = new PARENT(child1, child2);
        }
    }
    return result;
}

template<typename PARENT, typename CHILD>
static PARENT *parse_1_or_empty(parse_context_t *ctx) {
    PARENT *result = NULL;
    auto_ptr<CHILD> child(CHILD::parse(ctx));
    if (child.get()) {
        result = new PARENT(child);
    } else {
        result = new PARENT();
    }
    return result;
}

// Node visitor class, using CRTP. Child classes should override accept(
template<typename T>
struct node_visitor_t {
    /* Additional overrides */
    template<typename IGNORED_TYPE>
    void will_visit_children(const IGNORED_TYPE& t UNUSED) {}
    
    template<typename IGNORED_TYPE>
    void did_visit_children(const IGNORED_TYPE& t UNUSED) {}
    
    template<typename NODE_TYPE>
    void visit_internal(const NODE_TYPE &node)
    {
        T *derived_this = static_cast<T *>(this);
        derived_this->accept(node);
        derived_this->will_visit_children(node);
        node.visit_children(this);
        derived_this->did_visit_children(node);
    }
    
    
    /* Function called from overrides of visit_children. We invoke an override of accept(), and then recurse to children. */
    template<typename NODE_TYPE>
    void visit(const NODE_TYPE &t)
    {
        if (t.get() != NULL) {
            this->visit_internal(*t);
        }
    }
    
    /* Visit is called from node visit_children implementations. A token has no children. */
    void visit(const token_t &token) {
        static_cast<T *>(this)->accept(token);
    }
    
    /* Public entry point */
    template<typename ENTRY_TYPE>
    void begin(const ENTRY_TYPE &t) {
        this->visit_internal(t);
    }
};

/* Helper class for pretty-printing */
class node_dumper_t : public node_visitor_t<node_dumper_t> {
    string_t text;
    unsigned int depth;
    
    // Instances of this class are expected to be quite transient. src is unowned.
    const string_t *src;
    
    std::vector<std::string> lines;
    
    node_dumper_t(const string_t *src_ptr) : depth(0), src(src_ptr) {}
    
public:
    template<typename NODE_TYPE>
    void accept(const NODE_TYPE& node) {
        std::string result(2 * depth, ' ');
        result.append(node.name());
        lines.push_back(result);
    }
    
    template<typename NODE_TYPE>
    void will_visit_children(const NODE_TYPE &t) {
        depth = depth + 1 - t.unindent();
    }
    
    template<typename NODE_TYPE>
    void did_visit_children(const NODE_TYPE &t) {
        depth = depth - 1 + t.unindent();
    }
    
    
    void accept(const token_t &t1) {
        if (! t1.empty()) {
            std::string result(2 * depth, ' ');
            char buff[32];
            
            if (src != NULL) {
                const string_t word(*src, t1.range.start, t1.range.length);
                snprintf(buff, sizeof buff, "'%ls' ", widen(word).c_str());
            }
            result.append(buff);
            
            if (t1.range.length == 1) {
                snprintf(buff, sizeof buff, "{%lu}", t1.range.start);
            } else {
                snprintf(buff, sizeof buff, "{%lu-%lu}", t1.range.start, t1.range.length);
            }
            result.append(buff);
            lines.push_back(result);
        }
    }
    
    template<typename NODE_TYPE>
    static std::string dump_tree(const NODE_TYPE &node, const string_t &src) {
        node_dumper_t dumper(&src);
        dumper.begin(node);
        std::string result;
        for (size_t i=0; i < dumper.lines.size(); i++) {
            result.append(dumper.lines.at(i));
            result.push_back('\n');
        }
        dumper.src = NULL;
        return result;
    }
};


/* Helper class for collecting options from a tree */
template<typename NODE_TYPE>
struct node_collector_t : public node_visitor_t<node_collector_t<NODE_TYPE> > {
    std::vector<const NODE_TYPE *> results;
    
    // The requested type we capture
    void accept(const NODE_TYPE& node) {
        results.push_back(&node);
    }
    
    // Other types we ignore
    template<typename IGNORED_TYPE>
    void accept(const IGNORED_TYPE& t UNUSED) {}
};


/* Helper class for collecting all nodes of a given type */
template<typename ENTRY_TYPE, typename TYPE_TO_COLLECT>
std::vector<const TYPE_TO_COLLECT *> collect_nodes(const ENTRY_TYPE &entry) {
    node_collector_t<TYPE_TO_COLLECT> collector;
    collector.begin(entry);
    std::vector<const TYPE_TO_COLLECT *> result;
    result.swap(collector.results);
    return result;
}

/* Base class of all intermediate states */
struct base_t {
    // Which production was used
    uint8_t production;
    
    // Range of tokens used
    range_t token_range;
    
    // Constructors. The default production index is 0. The second constructor specifies a production.
    base_t() : production(0){}
    base_t(uint8_t p) : production(p) {}
    
    // How much to un-indent children when pretty-printing
    unsigned unindent() const { return 0; }
};

struct expression_list_t : public base_t {
    auto_ptr<expression_t> expression;
    auto_ptr<opt_expression_list_t> opt_expression_list;
    
    // expression_list = expression opt_expression_list
    expression_list_t(auto_ptr<expression_t> c1, auto_ptr<opt_expression_list_t> c2) : expression(c1), opt_expression_list(c2) {}
    static expression_list_t *parse(parse_context_t *ctx) {  return parse_2<expression_list_t, expression_t, opt_expression_list_t>(ctx); }
    std::string name() const { return "expression_list"; }
    
    template<typename T>
    void visit_children(T *v) const {
        v->visit(expression);
        v->visit(opt_expression_list);
    }
    
    /* Don't indent children of opt_expression_list to keep the list looking flatter */
    unsigned unindent() const { return 1*0; }
};

struct alternation_list_t : public base_t {
    auto_ptr<expression_list_t> expression_list;
    auto_ptr<or_continuation_t> or_continuation;
    
    alternation_list_t(auto_ptr<expression_list_t> el, auto_ptr<or_continuation_t> o) : expression_list(el), or_continuation(o) {}
    static alternation_list_t *parse(parse_context_t *ctx) { return parse_2<alternation_list_t, expression_list_t, or_continuation_t>(ctx); }
    std::string name() const { return "alternation_list"; }
    unsigned unindent() const { return 1*0; }
    
    template<typename T>
    void visit_children(T *v) const {
        v->visit(expression_list);
        v->visit(or_continuation);
    }

};

struct opt_expression_list_t : public base_t {
    auto_ptr<expression_list_t> expression_list;
    
    // opt_expression_list = empty
    opt_expression_list_t() {}
    
    // opt_expression_list = expression_list
    opt_expression_list_t(auto_ptr<expression_list_t> c) : base_t(1), expression_list(c) {}
    static opt_expression_list_t *parse(parse_context_t *ctx) { return parse_1_or_empty<opt_expression_list_t, expression_list_t>(ctx); }
    std::string name() const { return "opt_expression_list"; }
    unsigned unindent() const { return 1*0; }
    
    template<typename T>
    void visit_children(T *v) const {
        v->visit(expression_list);
    }
};

struct usage_t : public base_t {
    token_t prog_name;
    auto_ptr<alternation_list_t> alternation_list;
    auto_ptr<usage_t> next_usage;
    
    // usage = <empty>
    usage_t() : base_t(0) {}

    // usage = word usage
    usage_t(const token_t name, auto_ptr<usage_t> next) : base_t(1), prog_name(name), next_usage(next) {}
    
    // usage = word alternation_list usage
    usage_t(token_t t, auto_ptr<alternation_list_t> c, auto_ptr<usage_t> next) : base_t(2), prog_name(t), alternation_list(c), next_usage(next) {}
    
    static usage_t *parse(parse_context_t *ctx) {
        // If we reach the end, we return an empty usage
        if (ctx->is_at_end()) {
            return new usage_t();
        }
        
        usage_t *result = NULL;
        
        // TODO: generate error for missing word name
        token_t word;
        if (ctx->scan_word(&word)) {
            auto_ptr<alternation_list_t> el(alternation_list_t::parse(ctx));
            // Consume as many newlines as we can, and then try to generate the tail
            while (ctx->scan('\n')) {
                continue;
            }
            auto_ptr<usage_t> next(usage_t::parse(ctx));
            
            if (el.get()) {
                result = new usage_t(word, el, next);
            } else {
                result = new usage_t(word, next);
            }

        }
        return result;
    }
    
    std::string name() const { return "usage"; }
    
    template<typename T>
    void visit_children(T *v) const {
        v->visit(prog_name);
        v->visit(alternation_list);
        v->visit(next_usage);
    }
};

struct or_continuation_t : public base_t {
    token_t vertical_bar;
    auto_ptr<alternation_list_t> alternation_list;
    
    // or_continuation = <empty>
    or_continuation_t() {}
    
    // or_continuation =  VERT_BAR or_clause
    or_continuation_t(token_t b, auto_ptr<alternation_list_t> al) : base_t(1), vertical_bar(b), alternation_list(al) {}
    
    static or_continuation_t *parse(parse_context_t *ctx) {
        or_continuation_t *result = NULL;
        token_t bar;
        if (ctx->scan('|', &bar)) {
            auto_ptr<alternation_list_t> al(alternation_list_t::parse(ctx));
            if (al.get()) {
                result = new or_continuation_t(bar, al);
            } else {
                // TODO: generate an error about trailing bar
                fprintf(stderr, "Trailing bar\n");
            }
        } else {
            result = new or_continuation_t();
        }
        return result;
    }
    
    std::string name() const { return "or_continuation"; }
    
    template<typename T>
    void visit_children(T *v) const {
        v->visit(vertical_bar);
        v->visit(alternation_list);
    }
};

struct opt_ellipsis_t : public base_t {
    token_t ellipsis;

    // opt_ellipsis = <empty>
    // opt_ellipsis = ELLIPSIS
    opt_ellipsis_t(token_t t) : base_t(t.empty() ? 0 : 1), ellipsis(t) {}
    
    static opt_ellipsis_t *parse(parse_context_t *ctx) {
        token_t ellipsis;
        ctx->scan("...", &ellipsis);
        return new opt_ellipsis_t(ellipsis);
    }
    
    std::string name() const { return "opt_ellipsis"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(ellipsis);
    }
};

struct options_shortcut_t : public base_t {
    // The options shortcut does not need to remember its token, since we never use it
    options_shortcut_t(const token_t &t1 UNUSED) : base_t() {}
    
    static options_shortcut_t *parse(parse_context_t *ctx) {
        token_t opt;
        if (ctx->scan("[options]", &opt)) {
            return NULL;
        }
        return new options_shortcut_t(opt);
    }
    
    std::string name() const { return "options_shortcut"; }
    template<typename T>
    void visit_children(T *v UNUSED) const {}
};

struct simple_clause_t : public base_t {
    token_t word;
    
    simple_clause_t(const token_t &t) : word(t)
    {}
    
    static simple_clause_t *parse(parse_context_t *ctx) {
        simple_clause_t *result = NULL;
        token_t word;
        if (ctx->scan_word(&word)) {
            result = new simple_clause_t(word);
            
            /* Hack to support specifying parameter names inline.
             
             Consider usage like this:
               "usage: prog [-m <msg>]"
             
            There's two ways we can interpret this:
             1. An optional -m flag, followed by a positional parameter
             2. An -m option whose value is <msg>
            
            The Python reference docopt resolves this ambiguity by looking at the Options: section. If it finds a declaration of -m with a variable, then <msg> is assumed to be the value (interpretation #2); otherwise it's a positional (interpretation #1).
             
            But requiring a required positional after a flag seems very unlikely, so in this version we always use interpretation #2. We do this by treating it as one token '-m <msg>'. Note this token contains a space.
             
             The exception is if a delimeter is found, e.g.:
             
             "usage: prog [-m=<foo> <msg>]"
             
             Now <foo> is the value of the option m, and <msg> is positional.
             
             Also, if you really want interpretation #1, you can write:
             
               "usage: prog [-m (<msg>)]"
            */
            const string_t &src = *ctx->source;
            range_t range = word.range;
            if (range.length > 1 && src.at(range.start) == '-') {
                // It's an option
                option_t opt = parse_option_from_string(src, &range, NULL);
                if (opt.name.length > 0 && opt.separator == option_t::sep_space) {
                    // Looks like an option without a separator. See if the next token is a variable
                    range_t next = ctx->peek_word().range;
                    if (next.length > 2 && src.at(next.start) == '<' && src.at(next.end() - 1) == '>') {
                        // It's a variable
                        token_t variable;
                        bool scanned = ctx->scan_word(&variable);
                        assert(scanned); // Should always succeed, since we peeked at the word
                        word.range.merge(variable.range);
                    }
                }
            }
        }
        return result;
    }
    
    std::string name() const { return "simple_clause"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(word);
    }
    
};

struct expression_t : public base_t {
    // production 0
    auto_ptr<simple_clause_t> simple_clause;
    
    // Collapsed for productions 1 and 2
    token_t open_token;
    auto_ptr<alternation_list_t> alternation_list;
    token_t close_token;
    
    // Collapsed for all
    auto_ptr<opt_ellipsis_t> opt_ellipsis;
    
    auto_ptr<options_shortcut_t> options_shortcut;
    
    //expression = simple_clause
    expression_t(auto_ptr<simple_clause_t> c, auto_ptr<opt_ellipsis_t> e) : base_t(0), simple_clause(c), opt_ellipsis(e) {}
    
    //expression = OPEN_PAREN expression_list CLOSE_PAREN opt_ellipsis |
    //expression = OPEN_SQUARE expression_list CLOSE_SQUARE opt_ellipsis
    expression_t(token_t a, bool is_paren, auto_ptr<alternation_list_t> el, token_t b, auto_ptr<opt_ellipsis_t> e)
    : base_t(is_paren ? 1  : 2), open_token(a), alternation_list(el), close_token(b), opt_ellipsis(e)
    {}
    
    //expression = options_shortcut
    expression_t(auto_ptr<options_shortcut_t> os) : base_t(3), options_shortcut(os)
    {}
    
    static expression_t *parse(parse_context_t * ctx) {
        expression_t *result = NULL;
        if (ctx->is_at_end()) {
            return NULL;
        }
        
        token_t token;
        // Note that options must come before trying to parse it as a list
        
        if (ctx->scan("[options]", &token)) {
            auto_ptr<options_shortcut_t> shortcut(options_shortcut_t::parse(ctx));
            if (shortcut.get()) {
                result = new expression_t(shortcut);
            }
        } else if (ctx->scan('(', &token) || ctx->scan('[', &token)) {
            auto_ptr<alternation_list_t> contents(alternation_list_t::parse(ctx));
            if (contents.get()) {
                char_t c = ctx->source->at(token.range.start);
                assert(c == char_t('(') || c == char_t('['));
                bool is_paren = (c == char_t('('));
                token_t close_token;
                if (ctx->scan(char_t(is_paren ? ')' : ']'), &close_token)) {
                    auto_ptr<opt_ellipsis_t> ellipsis(opt_ellipsis_t::parse(ctx));
                    assert(ellipsis.get() != NULL); // should never fail
                    result = new expression_t(token, is_paren, contents, close_token, ellipsis);
                } else {
                    // TODO: generate error of unclosed paren
                }
            }
        } else if (ctx->scan("...", &token)) {
            // Indicates leading ellipsis
            // TODO: generate error
        } else {
            auto_ptr<simple_clause_t> simple_clause(simple_clause_t::parse(ctx));
            if (simple_clause.get()) {
                auto_ptr<opt_ellipsis_t> ellipsis(opt_ellipsis_t::parse(ctx));
                assert(ellipsis.get() != NULL); // should never fail
                result = new expression_t(simple_clause, ellipsis);
            }
        }
        
        return result;
    }
    
    std::string name() const { return "expression"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(simple_clause);
        v->visit(open_token);
        v->visit(alternation_list);
        v->visit(close_token);
        v->visit(opt_ellipsis);
        v->visit(options_shortcut);
    }
};

#pragma mark -



/* Class representing an error */
struct error_t {
    /* Where the error occurred */
    size_t location;

    /* Text of the error */
    string_t text;
};
typedef std::vector<error_t> error_list_t;

/* Constructor takes the source */
public:
const string_t source;
const parse_flags_t parse_flags;
docopt_impl(const string_t &s, parse_flags_t f) : source(s), parse_flags(f)
{}

/* Helper function to make a string (narrow or wide) from a C string */
static string_t string_from_narrow_string(const char *s) {
    string_t result;
    assign_narrow_string_to_string(s, &result);
    return result;
}

/* List of errors */
error_list_t errors;
static void append_error(error_list_t *errors, size_t where, const char *txt) {
    if (errors != NULL) {
        errors->resize(errors->size() + 1);
        error_t *error = &errors->back();
        error->location = where;
        assign_narrow_string_to_string(txt, &error->text);
    }
}


/* Helper typedefs */
typedef base_argument_t<string_t> arg_t;
typedef std::vector<string_t> string_list_t;

/* A positional argument */
struct positional_argument_t {
    size_t idx_in_argv;

    explicit positional_argument_t(size_t idx) : idx_in_argv(idx)
    {}
};
typedef std::vector<positional_argument_t> positional_argument_list_t;

/* A resolved option references an option in argv */
struct resolved_option_t {

    // The option referenced by this
    option_t option;
    
    // The index of the name portion of the option, in argv
    size_t name_idx_in_argv;
    
    // The index of the argument where the value was found. npos for none.
    size_t value_idx_in_argv;
    
    // The range within that argument where the value was found. This will be the entire string if the argument is separate (--foo bar) but will be the portion after the equals if not (--foo=bar, -Dfoo)
    range_t value_range_in_arg;

    resolved_option_t(const option_t &opt, size_t name_idx, size_t val_idx, const range_t &val_range) : option(opt), name_idx_in_argv(name_idx), value_idx_in_argv(val_idx), value_range_in_arg(val_range)
    {}
};
typedef std::vector<resolved_option_t> resolved_option_list_t;

/* Helper function to efficiently iterate over lines of a string 'source'. On input, inout_range.end() should be initialized to the start point for the iteration. On return, the range will contain the line, with its end pointing just after the trailing newline, or possibly at source.size(). The length of the line is thus the length of the range (and is guaranteed to be positive). Returns true if a line was returned, false if we reached the end.

    If end is specified, it is treated as the end of the string (i.e. str.size())
*/
static bool get_next_line(const string_t &str, range_t *inout_range, size_t end = string_t::npos)
{
    const size_t effective_end = std::min(str.size(), end);
    bool success = false;
    if (inout_range->end() < effective_end)
    {
        // Start at the end of the last line, or zero if this is the first call
        size_t line_start = inout_range->end();
        size_t line_end;
        size_t newline = str.find(char_t('\n'), line_start);
        if (newline > effective_end) {
            // Point just after the last character
            line_end = effective_end;
        } else {
            // Point just after the newline
            line_end = newline + 1;
        }
        // Empty lines are impossible
        assert(line_end > line_start);
        inout_range->start = line_start;
        inout_range->length = line_end - line_start;
        success = true;
    }
    return success;
}

/* Helper function for string containment. Returns true if c is found in cstr, which is ASCII. */
static bool contains(const char *cstr, char_t c) {
    assert(c != 0);
    for (size_t i=0; cstr[i]; i++) {
        if (cstr[i] == c) {
            return true;
        }
    }
    return false;
}

/* Returns true if the two strings are equal. */
static bool str_equals(const char *a, const string_t &str) {
    size_t len = str.size();
    const char_t *cstr = str.c_str();
    for (size_t i=0; i < len; i++) {
        // See if we've either reached the end of a, or the characters are different
        if (a[i] == '\0' || cstr[i] != a[i]) {
            return false;
        }
    }

    // We made it through. Ensure they have the same lengths
    // Note that we already dereferenced the previous characters of a
    return a[len] == '\0';
}

/* Helper function for string equality. Returns true if a and b are equal up to the first len characters */
static bool substr_equals(const char *a, const char_t *b, size_t len) {
    bool equals = true;
    for (size_t i=0; i < len; i++) {
        if (a[i] != b[i]) {
            equals = false;
            break;
        }
        if (a[i] == 0) {
            // End of string. Note that since these characters were equal, it's sufficient to test one of them.
            break;
        }
    }
    return equals;
}

/* Helper function for string equality. Returns true if a and b are equal up to the first len characters */
static bool substr_equals(const char *a, const string_t &b, size_t len) {
    return substr_equals(a, b.c_str(), len);
}

/* Returns true if the given token matches the given narrow string, up to len characters */
bool token_substr_equals(const token_t &tok, const char *str, size_t len) {
    assert(tok.range.end() <= this->source.length());
    if (len > tok.range.length) {
        /* If our token is too short, then it doesn't match */
        return false;
    } else {
        return substr_equals(str, this->source.c_str() + tok.range.start, len);
    }
}

/* Returns true if the given token matches the given string */
bool token_equals(const token_t &tok, const string_t &str) {
    assert(tok.range.end() < this->source.length());
    if (str.size() != tok.range.length) {
        /* If our token is too short, then it doesn't match */
        return false;
    } else {
        return this->source.compare(tok.range.start, tok.range.length, str) == 0;
    }
}

/* Collects options, i.e. tokens of the form --foo */
template<typename ENTRY_TYPE>
void collect_options_and_variables(const ENTRY_TYPE &entry, option_list_t *out_options, range_list_t *out_variables) {
    std::vector<const simple_clause_t *> nodes = collect_nodes<ENTRY_TYPE, simple_clause_t>(entry);
    for (size_t i=0; i < nodes.size(); i++) {
        const token_t &tok = nodes.at(i)->word;
        if (this->token_substr_equals(tok, "-", 1)) {
            out_options->push_back(parse_option_from_string(this->source, tok.range, &this->errors));
        } else if (this->token_substr_equals(tok, "<", 1)) {
            out_variables->push_back(tok.range);
        }
    }
}

/* Like parse_option_from_string, but parses an argument */
static option_t parse_option_from_argument(const string_t &str, error_list_t *errors UNUSED) {
    assert(! str.empty());
    assert(str.at(0) == char_t('-'));

    range_t remaining_storage(0, str.size());
    range_t * const remaining = &remaining_storage;
    
    // Count how many leading dashes
    range_t leading_dash_range = scan_while(str, remaining, it_equals<'-'>);
    
    // Walk over characters valid in a name
    range_t name_range = scan_while(str, remaining, char_is_valid_in_parameter);
    
    // Check to see if there's an = sign
    const range_t equals_range = scan_1_char(str, remaining, char_t('='));
    
    // If we got an equals sign, the rest is the value
    // Note that this is of a very different nature than char_is_valid_in_variable
    // It can have any character at all, since it's coming from the argument, not from the usage spec
    range_t value_range;
    if (! equals_range.empty()) {
        value_range = *remaining;
        remaining->start = remaining->end();
        remaining->length = 0;
    }
    
    // Return the option
    return option_t(name_range, value_range, leading_dash_range.length, equals_range.empty() ? option_t::sep_space : option_t::sep_equals);
}

static option_t parse_option_from_string(const string_t &str, range_t *remaining, error_list_t *errors UNUSED) {
    assert(remaining->length > 0);

    // Count how many leading dashes
    const size_t start = remaining->start;
    range_t leading_dash_range = scan_while(str, remaining, it_equals<'-'>);
    assert(leading_dash_range.length > 0);
    if (leading_dash_range.length > 2) {
        append_error(errors, start, "Too many dashes");
    }

    // Walk over characters valid in a name
    range_t name_range = scan_while(str, remaining, char_is_valid_in_parameter);
    
    // Check to see if there's a space
    range_t space_separator = scan_while(str, remaining, isspace);

    // Check to see if there's an = sign
    const range_t equals_range = scan_while(str, remaining, it_equals<'='>);
    if (equals_range.length > 1) {
        append_error(errors, equals_range.start, "Too many equal signs");
    }

    // Try to scan a variable
    // TODO: If we have a naked equals sign (foo = ) generate an error
    scan_while(str, remaining, isspace);
    
    range_t variable_range;
    range_t open_sign = scan_1_char(str, remaining, '<');
    if (! open_sign.empty()) {
        range_t variable_name_range = scan_while(str, remaining, char_is_valid_in_variable);
        range_t close_sign = scan_1_char(str, remaining, '>');
        if (! variable_name_range.empty() && ! close_sign.empty()) {
            variable_range.merge(open_sign);
            variable_range.merge(variable_name_range);
            variable_range.merge(close_sign);
        }
    }
    // TODO: generate error for wrong-looking variables
    
    // Determine the separator type
    // If we got an equals range, it's something like 'foo = <bar>' or 'foo=<bar>'. The separator is equals and the space is ignored.
    // Otherwise, the space matters: 'foo <bar>' is space-separated, and 'foo<bar>' has no separator
    // Hackish: store sep_space for options without a separator
    option_t::separator_t separator;
    if (variable_range.empty()) {
        separator = option_t::sep_space;
    } else if (! equals_range.empty()) {
        separator = option_t::sep_equals;
    } else if (! space_separator.empty()) {
        separator = option_t::sep_space;
    } else {
        separator = option_t::sep_none;
    }
    
    // TODO: generate an error on long options with no separators (--foo<bar>). Only short options support these.
    if (separator == option_t::sep_none && (leading_dash_range.length > 1 || name_range.length > 1)) {
        append_error(errors, name_range.start, "Long options must use a space or equals separator");
    }
    
    // Create and return the option
    return option_t(name_range, variable_range, leading_dash_range.length, separator);
}

/* Variant for the non-options-spec case */
option_t parse_option_from_string(const string_t &str, range_t range, error_list_t *errors) const {
    return parse_option_from_string(str, &range, errors);
}

/* Given an option spec in the given range, that extends from the initial - to the end of the description, parse out a list of options */
option_list_t parse_one_option_spec(const range_t &range, error_list_t *errors) const {
    assert(! range.empty());
    assert(this->source.at(range.start) == char_t('-'));
    const size_t end = range.end();

    option_list_t result;

    // Look for two spaces. Those separate the description.
    // This is a two-space "C-string"
    const char_t two_spaces[] = {char_t(' '), char_t(' '), char_t('\0')};
    size_t options_end = this->source.find(two_spaces, range.start);
    if (options_end > end) {
        options_end = end; // no description
    }

    // Determine the description range (possibly empty). Skip over its leading whitespace
    range_t description_range = range_t(options_end, end - options_end);
    scan_while(this->source, &description_range, isspace);
    
    // Parse out a "default:" value.
    range_t default_value_range;
    if (! description_range.empty()) {
        // TODO: handle the case where there's more than one
        const char *default_prefix = "[default:";
        size_t default_prefix_loc = find_case_insensitive(this->source, default_prefix, description_range.start);
        if (default_prefix_loc < description_range.end()) {
            // Note: the above check handles npos too
            size_t default_value_start = default_prefix_loc + strlen(default_prefix);
            // Skip over spaces
            while (default_value_start < description_range.end() && isspace(this->source.at(default_value_start))) {
                default_value_start++;
            }
            
            // Find the closing ']'
            size_t default_value_end = this->source.find(char_t(']'), default_value_start);
            if (default_value_end >= description_range.end()) {
                // Note: The above check covers npos too
                append_error(errors, default_prefix_loc, "Missing ']'");
            } else {
                default_value_range.start = default_value_start;
                default_value_range.length = default_value_end - default_value_start;
            }
        }
    }

    // Parse the options portion
    assert(options_end >= range.start);
    range_t remaining(range.start, options_end - range.start);
    scan_while(this->source, &remaining, isspace);
    range_t name_range_of_last_long_option;
    while (! remaining.empty()) {
        option_t opt = parse_option_from_string(this->source, &remaining, errors);
        if (opt.name.empty()) {
            // Failed to get an option, give up
            break;
        }
        opt.description_range = description_range;
        opt.default_value_range = default_value_range;
        result.push_back(opt);
        
        // Keep track of the last long option
        if (opt.type == option_t::double_long) {
            name_range_of_last_long_option = opt.name;
        }
        
        // Skip over commas, which separate arguments
        scan_while(this->source, &remaining, isspace);
        scan_while(this->source, &remaining, it_equals<','>);
        scan_while(this->source, &remaining, isspace);

    }
    
    // Set the key range to the name range of every option
    if (! name_range_of_last_long_option.empty()) {
        for (size_t i=0; i < result.size(); i++) {
            result.at(i).key_range = name_range_of_last_long_option;
        }
    }
    
    return result;
}

// Returns true if the source line contains an option spec, that is, has at least one leading space, and then a dash
static bool line_contains_option_spec(const string_t &str, const range_t &range) {
    range_t remaining = range;
    range_t space = scan_while(str, &remaining, isspace);
    range_t dashes = scan_while(str, &remaining, it_equals<'-'>);
    return space.length > 0 && dashes.length > 0;
}

/* Finds the headers containing name (for example, "Options:") and returns source ranges for them. Header lines are not included. */
range_list_t source_ranges_for_section(const char *name) const {
    range_list_t result;
    bool in_desired_section = false;
    range_t line_range;
    while (get_next_line(this->source, &line_range)) {
        size_t line_start = line_range.start;
        // It's a header line if the first character is not whitespace
        bool is_header = ! isspace(source.at(line_start));
        if (is_header) {
            // Check to see if the name is found before the first colon
            // Note that if name is not found at all, name_pos will have value npos, which is huge (and therefore not smaller than line_end)
            size_t name_pos = find_case_insensitive(source, name, line_start);
            size_t line_end = line_range.end();
            in_desired_section = (name_pos < line_end && name_pos < source.find(':', line_start));

            if (in_desired_section) {
                // Append a new empty range. We will add to it.
                result.push_back(range_t(0, 0));
                
                // Adjust this range to start after the header name
                size_t line_end = line_range.end();
                size_t new_start = name_pos + strlen(name);
                assert(new_start <= line_end);
                line_range.start = new_start;
                line_range.length = line_end - new_start;
            }
        }

        if (in_desired_section) {
            // Extend the last range with this line
            result.back().merge(line_range);
        }
    }
    return result;
}

/* Parses the option specification at the given location */
option_list_t parse_options_spec(error_list_t *errors) const {
    option_list_t result;

    const range_list_t section_ranges = source_ranges_for_section("Options:");
    for (size_t range_idx = 0; range_idx < section_ranges.size(); range_idx++) {
        const range_t section_range = section_ranges.at(range_idx);
        const size_t section_end = section_range.end();
        range_t line_range(section_range.start, 0);
        while (get_next_line(this->source, &line_range, section_end)) {
            // These are all valid option specifications:
            // --foo
            // --foo <bar>
            // --foo=<bar>
            // --foo=<bar>, -f=<bar>
            // There may also be a description after two spaces
            // The description may span multiple lines.

            // Check to see if this line starts with a -
            if (line_contains_option_spec(this->source, line_range)) {
                // It's a new option. Determine how long its description goes.
                range_t option_spec_range = line_range;
                range_t local_range = line_range;
                while (get_next_line(this->source, &local_range, section_end)) {
                    if (line_contains_option_spec(this->source, local_range)) {
                        break;
                    } else {
                        option_spec_range.merge(local_range);
                        // Set our outermost lines to this line, so we'll skip past it next iteration
                        line_range = local_range;
                    }
                }

                // Got the description. Skip leading whitespace and then parse out an option.
                scan_while(this->source, &option_spec_range, isspace);
                option_list_t tmp = parse_one_option_spec(option_spec_range, errors);
                result.insert(result.end(), tmp.begin(), tmp.end());
            }
        }
    }
    return result;
}

/* Returns true if the two options have the same name */
bool options_have_same_name(const option_t &opt1, const option_t &opt2) const {
    bool result = false;
    if (opt1.name.length == opt2.name.length) {
        // Name lengths must be the same
        if (opt1.name == opt2.name) {
            // Identical ranges
            result = true;
        } else {
            result = (0 == this->source.compare(opt1.name.start, opt1.name.length, this->source, opt2.name.start, opt2.name.length));
        }
    }
    return result;
}

/* Given a list of options, verify that any duplicate options are in agreement, and remove all but one. TODO: make this not N^2. */
void uniqueize_options(option_list_t *options, error_list_t *errors UNUSED) const {
    std::vector<size_t> matching_indexes;
    for (size_t cursor=0; cursor < options->size(); cursor++) {
        // Determine the set of matches
        // Two options are called a match if they have the same name
        matching_indexes.push_back(cursor);
        // We have a "best option cursor". If we find two matching options, pick the one with the better (longer) description.
        size_t best_match_idx = cursor;
        for (size_t match_cursor = cursor + 1; match_cursor < options->size(); match_cursor++) {
            const option_t &current_match = options->at(best_match_idx);
            const option_t &maybe_match = options->at(match_cursor);
            if (this->options_have_same_name(current_match, maybe_match)) {
                // This index matched
                matching_indexes.push_back(match_cursor);
                
                // This argument matches.
                // TODO: verify agreement in the parameters, etc.
                if (maybe_match.description_range.length > current_match.description_range.length) {
                    // The second one has a better description. Keep it.
                    best_match_idx = match_cursor;
                }
            }
        }
        
        // Now we have the set of matching indexes
        // Erase all, except for the best match
        // These are in ascending order, so we can just work backwards
        while (! matching_indexes.empty()) {
            size_t idx_to_remove = matching_indexes.back();
            assert(idx_to_remove >= cursor); //should only remove at or after cursor
            matching_indexes.pop_back();
            if (idx_to_remove != best_match_idx) {
                options->erase(options->begin() + idx_to_remove);
                
                // If we removed the cursor, step back one so we go to the next element next time
                if (idx_to_remove == cursor) {
                    cursor -= 1;
                }
            }
        }
    }
}

/* Extracts a long option from the arg at idx, and appends the result to out_result. Updates idx. */
bool parse_long(const string_list_t &argv, option_t::type_t type, size_t *idx, const option_list_t &options, resolved_option_list_t *out_result, error_list_t *out_errors) {
    const string_t &arg = argv.at(*idx);
    assert(type == option_t::single_long || type == option_t::double_long);
    assert(substr_equals("--", arg, (type == option_t::double_long ? 2 : 1)));

    /* Parse the argument into an 'option'. Note that this option does not appear in the options list because its range reflects the string in the argument. TODO: Need to distinguish between equivalent ways of specifying parameters (--foo=bar and --foo bar) */
    error_list_t local_errors;
    option_t arg_as_option = parse_option_from_argument(arg, &local_errors);
    
    // Hacktastic - parse_option_from_string can't distinguish between one-char long options, and short options. So force the issue: if we want a single long option but we get a single short, then stomp it.
    if (type == option_t::single_long && arg_as_option.type == option_t::single_short) {
        arg_as_option.type = option_t::single_long;
    }
    
    // TODO: What if we get an error, e.g. there's more than two dashes?
    assert(arg_as_option.type == type);

    /* Get list of matching long options. */
    option_list_t matches;
    for (size_t i=0; i < options.size(); i++) {
        const option_t &opt = options.at(i);
        // This comparison is terrifying. It's just comparing two substrings: one in source (the given option) and the name portion of the argument
        if (opt.type == type && this->source.compare(opt.name.start, opt.name.length, arg, arg_as_option.name.start, arg_as_option.name.length) == 0) {
            matches.push_back(opt);
        }
    }
    
    if (matches.empty() && (this->parse_flags & flag_resolve_unambiguous_prefixes)) {
        /* We didn't get any direct matches; look for an unambiguous prefix match */
        option_list_t prefix_matches;
        for (size_t i=0; i < options.size(); i++) {
            const option_t &opt = options.at(i);
            // Here we confirm that the option's name is longer than the name portion of the argument.
            // If they are equal; we would have had an exact match above; if the option is shorter, then the argument is not a prefix of it.
            // If the option is longer, we then do a substring comparison, up to the number of characters determined by the argument
            if (opt.type == type && opt.name.length > arg_as_option.name.length && this->source.compare(opt.name.start, arg_as_option.name.length, arg, arg_as_option.name.start, arg_as_option.name.length) == 0) {
                prefix_matches.push_back(opt);
            }
        }
        if (prefix_matches.size() > 1) {
            // Todo: need to specify the argument index
            append_error(out_errors, -1, "Option specified too many times");
        } else if (prefix_matches.size() == 1) {
            // We have one unambiguous prefix match. Swap it into the true matches array, which is currently empty.
            matches.swap(prefix_matches);
        } else {
            // Empty, no prefix match at all. Continue on.
        }
    }

    /* TODO: handle unambiguous prefix */
    /* TODO: Better error reporting */
    /* TODO: can eliminate matches array entirely, just use a single index */

    bool success = false;
    size_t match_count = matches.size();
    if (match_count > 1) {
        append_error(out_errors, matches.at(0).name.start, "Option specified too many times");
    } else if (match_count < 1) {
        append_error(out_errors, -1, "Unknown option");
    } else {
        bool errored = false;
        assert(match_count == 1);
        const option_t &match = matches.at(0);

        /* Ensure the option and argument agree on having a value */
        range_t value_range(0, 0);
        const size_t name_idx = *idx;
        size_t arg_index = npos;
        if (match.has_value()) {
            if (arg_as_option.has_value()) {
                // The arg was specified as --foo=bar. The range is the value portion; the index is the same as our argument.
                value_range = arg_as_option.value;
                arg_index = *idx;
            } else {
                // The arg was (hopefully) specified as --foo bar
                // The index is of the next argument, and the range is the entire argument
                if (*idx + 1 < argv.size()) {
                    *idx += 1;
                    arg_index = *idx;
                    value_range = range_t(0, argv.at(arg_index).size());
                } else {
                    append_error(&errors, match.value.start, "Option expects an argument");
                    errored = true;
                }
            }
        } else if (arg_as_option.has_value()) {
            // A value was specified as --foo=bar, but none was expected
            append_error(&errors, match.name.start, "Option does not expect an argument");
            errored = true;
        }
        if (! errored) {
            out_result->push_back(resolved_option_t(match, name_idx, arg_index, value_range));
            *idx += 1;
            success = true;
        }
    }
    return success;
}

// Given a list of short options, try parsing out an unseparated short, i.e. -DNDEBUG. We only look at short options with no separator.
bool parse_unseparated_short(const string_list_t &argv, size_t *idx, const option_list_t &options, resolved_option_list_t *out_result, error_list_t *out_errors) {
    const string_t &arg = argv.at(*idx);
    assert(substr_equals("-", arg, 1));
    assert(arg.size() > 1); // must not be just a single dash
    bool success = false;
    
    // Construct the list of options in-order, corresponding to this argument
    std::vector<option_t> matches;
    
    for (size_t i=0; i < options.size(); i++) {
        const option_t &opt = options.at(i);
        if (opt.type == option_t::single_short && opt.separator == option_t::sep_none) {
            // Candidate short option.
            // This looks something like -DNDEBUG. We want to see if the D matches.
            // Compare the character at offset 1 (to account for the dash) and length 1 (since it's a short option)
            if (this->source.compare(opt.name.start, opt.name.length, arg, 1, 1) == 0) {
                // Expect to always want a value here
                assert(opt.has_value());
                matches.push_back(opt);
            }
        }
    }

    if (matches.size() > 1) {
        // This is an error in the usage - should catch this earlier?
        append_error(out_errors, matches.at(0).name.start, "Option specified too many times");
    } else if (matches.size() == 1) {
        // Try to extract the value. This is very simple: it starts at index 2 and goes to the end of the arg.
        const option_t &match = matches.at(0);
        if (arg.size() <= 2) {
            append_error(out_errors, match.name.start, "Option expects an argument");
        } else {
            // Got one
            size_t name_idx = *idx;
            size_t value_idx = *idx;
            range_t value_range = range_t(2, arg.size() - 2);
            out_result->push_back(resolved_option_t(match, name_idx, value_idx, value_range));
            *idx += 1;
            success = true;
        }
    } else {
        // Common case: Match count is 0, so we didn't get any.
        assert(matches.empty());
    }
    return success;
}


// Given a list of short options, parse out an argument
bool parse_short(const string_list_t &argv, size_t *idx, const option_list_t &options, resolved_option_list_t *out_result, error_list_t *out_errors) {
    const string_t &arg = argv.at(*idx);
    assert(substr_equals("-", arg, 1));
    assert(arg.size() > 1); // must not be just a single dash
    bool errored = false;
    bool last_option_has_argument = false;
    
    // Construct the list of options in-order, corresponding to this argument
    std::vector<option_t> options_for_argument;
    
    std::vector<option_t> matches;
    for (size_t idx_in_arg=1; idx_in_arg < arg.size() && ! errored; idx_in_arg++) {
        /* Get list of short options matching this resolved option. These are pointers into our options array */
        matches.clear();
        for (size_t i=0; i < options.size(); i++) {
            const option_t &opt = options.at(i);
            // This comparison is terrifying. It's just comparing two substrings: one in source (the given option) and the name portion of the argument. We pass 1 because the length of the string i1.
            if (opt.type == option_t::single_short && this->source.compare(opt.name.start, opt.name.length, arg, idx_in_arg, 1) == 0) {
                matches.push_back(opt);
            }
        }
        
        size_t match_count = matches.size();
        if (match_count > 1) {
            append_error(out_errors, matches.at(0).name.start, "Option specified too many times");
            errored = true;
        } else if (match_count < 1) {
            append_error(out_errors, -1, "Unknown option");
            errored = true;
        } else {
            // Just one match, add it to the global array
            options_for_argument.push_back(matches.at(0));
        }
    }
    
    if (! errored) {
        // Now we have all of the short options that this argument represents. No option is allowed to have a value, except possibly last one.
        for (size_t i=0; i < options_for_argument.size(); i++) {
            const option_t &opt = options_for_argument.at(i);
            if (opt.has_value()) {
                if (i + 1 == options_for_argument.size()) {
                    // This is the last option
                    last_option_has_argument = true;
                } else {
                    // This is not the last option
                    // TODO: this location (opt.name.start) is the location in the usage doc. It ought to be the location in the argument.
                    append_error(out_errors, opt.name.start, "Option requires an argument");
                }
            }
        }
    }
    
    // If we have an argument, determine its index
    range_t val_range_for_last_option(0, 0);
    const size_t name_idx = *idx;
    size_t val_idx_for_last_option = npos;
    if (! errored && last_option_has_argument) {
        // We don't support -f=bar style. I don't know of any commands that use this.
        // TODO: support delimiter-free style (gcc -Dmacro=something)
        if (*idx + 1 < argv.size()) {
            val_idx_for_last_option = *idx + 1;
            val_range_for_last_option = range_t(0, argv.at(*idx + 1).size());
        } else {
            append_error(&errors, options_for_argument.back().value.start, "Option expects an argument");
            errored = true;
        }
    }
    
    if (! errored) {
        // Construct resolved options
        for (size_t i=0; i < options_for_argument.size(); i++) {
            // Most options have no value.
            size_t val_idx = npos;
            range_t val_range(0, 0);
            if (i + 1 == options_for_argument.size() && last_option_has_argument) {
                // This is the last option
                val_range = val_range_for_last_option;
                val_idx = val_idx_for_last_option;
            }
            const option_t &opt = options_for_argument.at(i);
            out_result->push_back(resolved_option_t(opt, name_idx, val_idx, val_range));
        }
        
        // Update the index
        *idx += (last_option_has_argument ? 2 : 1);
    }
    return ! errored;
}

/* The Python implementation calls this "parse_argv" */
void separate_argv_into_options_and_positionals(const string_list_t &argv, const option_list_t &options, positional_argument_list_t *out_positionals, resolved_option_list_t *out_resolved_options, bool options_first UNUSED = false) {

    size_t idx = 0;
    while (idx < argv.size()) {
        const string_t arg = argv.at(idx);
        if (str_equals("--", arg)) {
            // Literal --. The remaining arguments are positional. Insert everything remaining and exit early
            while (++idx < argv.size()) {
                out_positionals->push_back(positional_argument_t(idx));
            }
            break;
        } else if (substr_equals("--", arg, 2)) {
            // Leading long option
            if (parse_long(argv, option_t::double_long, &idx, options, out_resolved_options, &this->errors)) {
                // parse_long will have updated idx and out_resolved_options
            } else {
                // This argument is unused
                // We have to update idx
                idx += 1;
            }
        } else if (substr_equals("-", arg, 1) && arg.size() > 1) {
            /* An option with a leading dash, like -foo
             This can be a lot of different things:
               1. A combined short option: tar -cf ...
               2. A long option with a single dash: -std=c++
               3. A short option with a value: -DNDEBUG
             Try to parse it as a long option; if that fails try to parse it as a short option
             */
            if (parse_long(argv, option_t::single_long, &idx, options, out_resolved_options, &this->errors)) {
                // parse_long succeeded. TODO: implement this.
            } else if (parse_unseparated_short(argv, &idx, options, out_resolved_options, &this->errors)) {
                // parse_unseparated_short will have updated idx and out_resolved_options
            } else if (parse_short(argv, &idx, options, out_resolved_options, &this->errors)) {
                // parse_short succeeded.
            } else {
                // Unparseable argument
                idx += 1;
            }
        } else {
            // Positional argument
            // Note this includes just single-dash arguments, which are often a stand-in for stdin
            out_positionals->push_back(positional_argument_t(idx));
            idx += 1;
        }
    }
}

/* The result of parsing argv */
typedef std::map<string_t, base_argument_t<string_t> > option_map_t;
typedef std::vector<option_map_t> option_map_list_t;

struct match_state_t {
    option_map_t option_map;
    size_t next_positional_index;

    match_state_t() : next_positional_index(0)
    {}

    void swap(match_state_t &rhs) {
        this->option_map.swap(rhs.option_map);
        std::swap(this->next_positional_index, rhs.next_positional_index);
    }
};

typedef std::vector<match_state_t> match_state_list_t;

static match_state_list_t no_match() { return match_state_list_t(); }

struct match_context_t {
    /* Note: these are stored references. Match context objects are expected to be transient and stack-allocated. */
    const positional_argument_list_t &positionals;
    const resolved_option_list_t &resolved_options;
    const option_list_t &shortcut_options;
    const string_list_t &argv;
    

    bool has_more_positionals(const match_state_t *state) const {
        assert(state->next_positional_index <= this->positionals.size());
        return state->next_positional_index < this->positionals.size();
    }
    
    // Returns the indexes in argv of the arguments that were unused
    index_list_t unused_arguments(const match_state_t *state, const string_t &src) const {
        /* To find the unused arguments, we walk over the used arguments and take what's left
           Arguments may be unused for any of three reasons:
             1. It is an unconsumed positional
             2. It is an option that we found in the tree, but was not matched during tree descent
             3. It is an option that was not found in the tree at all
        */
        
        /* Make a vector the same size as argv. As we walk over positionals and options, we will mark the corresponding index as used. At the end, the unset bits are the unused arguments */
        std::vector<bool> used_indexes(this->argv.size(), false);
        
        /* Walk over used positionals. next_positional_index is the first unused one.  */
        for (size_t i=0; i < state->next_positional_index; i++) {
            used_indexes.at(this->positionals.at(i).idx_in_argv) = true;
        }
        
        /* Walk over options matched during tree descent */
        string_t name;
        for (size_t i=0; i < resolved_options.size(); i++) {
            const resolved_option_t &opt = resolved_options.at(i);
            opt.option.assign_key_to_string(src, &name);
            if (state->option_map.find(name) != state->option_map.end()) {
                // This option was used. The name index is definitely used. The value index is also used, if it's not npos (note that it may be the same as the name index, so we can avoid setting the bit twice in that case)
                used_indexes.at(opt.name_idx_in_argv) = true;
                if (opt.value_idx_in_argv != npos && opt.value_idx_in_argv != opt.name_idx_in_argv) {
                    used_indexes.at(opt.value_idx_in_argv) = true;
                }
            }
        }
        
        /* Extract the unused indexes from the bitmap of used arguments */
        index_list_t unused_argv_idxs;
        for (size_t i=0; i < used_indexes.size(); i++) {
            if (! used_indexes.at(i)) {
                unused_argv_idxs.push_back(i);
            }
        }
        
        return unused_argv_idxs;
    }

    const positional_argument_t &next_positional(match_state_t *state) const {
        assert(state->next_positional_index < positionals.size());
        return positionals.at(state->next_positional_index);
    }

    const positional_argument_t &acquire_next_positional(match_state_t *state) const {
        assert(state->next_positional_index < positionals.size());
        return positionals.at(state->next_positional_index++);
    }

    match_context_t(const positional_argument_list_t &p, const resolved_option_list_t &r, const option_list_t &sco, const string_list_t &av) : positionals(p), resolved_options(r), shortcut_options(sco), argv(av)
    {}
};

static void state_list_destructive_append_to(match_state_list_t *source, match_state_list_t *dest) {
    size_t src_count = source->size();
    size_t dst_init_count = dest->size();

    // Add a bunch of empty maps to the destination
    dest->resize(dst_init_count + src_count);

    // Swap corresponding empty maps
    for (size_t i=0; i < src_count; i++) {
        match_state_t &src_state = source->at(i);
        match_state_t &dst_state = dest->at(i + dst_init_count);
        dst_state.swap(src_state);
    }

    // Clean up source since it's now useless
    source->clear();
}

static void state_destructive_append_to(match_state_t *state, match_state_list_t *dest) {
    dest->resize(dest->size() + 1);
    dest->back().swap(*state);
}

template<typename T>
match_state_list_t try_match(T& ptr, match_state_t *state, const match_context_t *ctx) {
    match_state_list_t result;
    if (ptr.get()) {
        result = this->match(*ptr, state, ctx);
    }
    return result;
}

template<typename T>
match_state_list_t try_match(T& ptr, match_state_list_t &state_list, const match_context_t *ctx) {
    match_state_list_t total_result;
    if (ptr.get()) {
        for (size_t i=0; i < state_list.size(); i++) {
            match_state_t *state = &state_list.at(i);
            match_state_list_t child_result = this->match(*ptr, state, ctx);
            state_list_destructive_append_to(&child_result, &total_result);
        }
    }
    return total_result;
}

/* Match overrides */
match_state_list_t match(const usage_t &node, match_state_t *state, const match_context_t *ctx) {
    if (! ctx->has_more_positionals(state)) {
        // todo: error handling
        return no_match();
    }
    
    if (node.prog_name.empty()) {
        // This is the final terminating usage. It does not match anything.
        return no_match();
    }
    
    /* Must duplicate the state for the next usage */
    match_state_t copied_state = *state;
    
    // Program name
    ctx->acquire_next_positional(state);
    
    match_state_list_t main_branch;
    if (node.alternation_list.get()) {
        // Match against our contents
        main_branch = try_match(node.alternation_list, state, ctx);
    } else {
        // Usage is just the program name
        // Merely append this state
        state_destructive_append_to(state, &main_branch);
    }
    match_state_list_t next_branch = try_match(node.next_usage, &copied_state, ctx);
    state_list_destructive_append_to(&next_branch, &main_branch);
    return main_branch;
}

match_state_list_t match(const expression_list_t &node, match_state_t *state, const match_context_t *ctx) {
    match_state_list_t intermed_state_list = try_match(node.expression, state, ctx);
    return try_match(node.opt_expression_list, intermed_state_list, ctx);
}

match_state_list_t match(const opt_expression_list_t &node, match_state_t *state, const match_context_t *ctx) {
    match_state_list_t result;
    if (node.expression_list.get()) {
        return match(*node.expression_list, state, ctx);
    } else {
        // end of the list
        return match_state_list_t(1, *state);
    }
}

match_state_list_t match(const alternation_list_t &node, match_state_t *state, const match_context_t *ctx) {
    // Must duplicate the state for the second branch
    match_state_t copied_state = *state;
    match_state_list_t result1 = try_match(node.expression_list, state, ctx);
    match_state_list_t result2 = try_match(node.or_continuation, &copied_state, ctx);

    // Combine the two lists into result1
    state_list_destructive_append_to(&result2, &result1);
    return result1;
}

match_state_list_t match(const or_continuation_t &node, match_state_t *state, const match_context_t *ctx) {
    return try_match(node.alternation_list, state, ctx);
}

match_state_list_t match(const expression_t &node, match_state_t *state, const match_context_t *ctx) {
    // Check to see if we have ellipsis. If so, we keep going as long as we can.
    bool has_ellipsis = (node.opt_ellipsis.get() != NULL && node.opt_ellipsis->production > 0);
    match_state_list_t result;

    switch (node.production) {
        case 0:
        {
            /* This is a simple clause which may have ellipsis, like foo...
               If we have ellipsis, we match one time, and then construct a sequence of 'intermediate state lists'.
               An intermediate state represents the result of matching N times. Each time we construct
               a new intermediate state, we append (copy) all of its states into the result; thus we may
               match one time, two times, three times...
               We stop when we get no more matches, which usually happens when we run out of positionals.
            */
            result = try_match(node.simple_clause, state, ctx);
            if (has_ellipsis) {
                match_state_list_t intermediate_states = result;
                while (! intermediate_states.empty()) {
                    match_state_list_t next_states = try_match(node.simple_clause, intermediate_states, ctx);
                    result.insert(result.end(), next_states.begin(), next_states.end());
                    intermediate_states.swap(next_states);
                }
            }
            break;
        }

        case 1:
        {
            /* This is a parenthesized clause which may have ellipsis, like (foo)...
               Same algorithm as the simple clause above.
               TODO: this may loop forever with states that do not consume any values, e.g. ([foo])...
            */
            result = try_match(node.alternation_list, state, ctx);
            if (has_ellipsis) {
                match_state_list_t intermediate_states = result;
                while (! intermediate_states.empty()) {
                    match_state_list_t next_states = try_match(node.alternation_list, intermediate_states, ctx);
                    result.insert(result.end(), next_states.begin(), next_states.end());
                    intermediate_states.swap(next_states);
                }
            }
            break;
        }

        case 2:
        {
            /* This is a square-bracketed clause which may have ellipsis, like [foo]...
               Same algorithm as the simple clause above, except that we also append the initial state as a not-taken branch.
            */
            match_state_t not_taken_branch = *state;
            result = try_match(node.alternation_list, state, ctx);
            if (has_ellipsis) {
                match_state_list_t intermediate_states = result;
                while (! intermediate_states.empty()) {
                    match_state_list_t next_states = try_match(node.alternation_list, intermediate_states, ctx);
                    result.insert(result.end(), next_states.begin(), next_states.end());
                    intermediate_states.swap(next_states);
                }
            }
            // Append the not taken branch
            state_destructive_append_to(&not_taken_branch, &result);
            break;
        }

        case 3:
        {
            // This is the [options] clause. It does not have ellipsis.
            result = this->match_options(ctx->shortcut_options, false /* don't require match */, state, ctx);
            break;
        }

        default:
            assert(0 && "unknown production");
            return no_match();
    }

    return result;
}

// Match the options in the options list, updating the state
// This returns success (i.e. a non-empty state list) if we match at least one
match_state_list_t match_options(const option_list_t &options_in_doc, bool require__match, match_state_t *state, const match_context_t *ctx) {
    match_state_list_t result;
    size_t successful_matches = 0;
    for (size_t j=0; j < options_in_doc.size(); j++) {
        const option_t opt_in_doc = options_in_doc.at(j);

        // Find the matching option from the option list
        const resolved_option_t *resolved_opt = NULL;
        for (size_t i=0; i < ctx->resolved_options.size(); i++) {
            const resolved_option_t *opt_in_argv = &ctx->resolved_options.at(i);
            if (options_have_same_name(opt_in_argv->option, opt_in_doc)) {
                resolved_opt = opt_in_argv;
                break;
            }
        }

        if (resolved_opt != NULL) {
            // woo hoo
            const string_t name = opt_in_doc.key_as_string(this->source);
            arg_t *arg = &state->option_map[name];
            if (resolved_opt->value_idx_in_argv != npos) {
                const string_t &str = ctx->argv.at(resolved_opt->value_idx_in_argv);
                const range_t value_range = resolved_opt->value_range_in_arg;
                arg->values.push_back(string_t(str, value_range.start, value_range.length));
            }
            arg->count += 1;
            successful_matches += 1;
        }
    }
    
    if (! require__match || successful_matches > 0) {
        state_destructive_append_to(state, &result);
    }
    return result;
}

match_state_list_t match(const simple_clause_t &node, match_state_t *state, const match_context_t *ctx) {
    // Check to see if this is an argument or a variable
    match_state_list_t result;
    const range_t &range = node.word.range;
    char_t first_char = this->source.at(range.start);

    if (first_char == char_t('<')) {
        if (ctx->has_more_positionals(state)) {
            // Variable argument. Modify the state.
            // Note we retain the brackets <> in the variable name
            const string_t name = string_t(this->source, range.start, range.length);
            arg_t *arg = &state->option_map[name];
            const positional_argument_t &positional = ctx->acquire_next_positional(state);
            arg->values.push_back(ctx->argv.at(positional.idx_in_argv));
            state_destructive_append_to(state, &result);
        }
    } else if (first_char == char_t('-')) {
        // Option
        option_list_t options_in_doc;
        options_in_doc.push_back(parse_option_from_string(this->source, range, NULL));
        result = this->match_options(options_in_doc, true /* require_match */, state, ctx);
    } else {
        // Fixed argument
        if (ctx->has_more_positionals(state)) {
            const positional_argument_t &positional = ctx->next_positional(state);
            const string_t &name = ctx->argv.at(positional.idx_in_argv);
            string_t tmp = string_t(this->source, range.start, range.length);
            if (name.size() == range.length && this->source.compare(range.start, range.length, name) == 0) {
                // The fixed argument matches
                arg_t *arg = &state->option_map[name];
                arg->values.push_back(string_t(1, char_t('1')));
                arg->count += 1;
                ctx->acquire_next_positional(state);
                state_destructive_append_to(state, &result);
            }
        }
    }
    return result;
}

option_map_t finalize_option_map(const option_map_t &map, const option_list_t &all_options, const range_list_t &all_variables) {
    // If we aren't asked to do empty args, then skip it
    if (! (this->parse_flags & flag_generate_empty_args)) {
        return map;
    }
    
    // For each option, fill in the value in the map
    // This could be made more efficient via a single call to insert()
    option_map_t result = map;
    string_t name;
    for (size_t i=0; i < all_options.size(); i++) {
        const option_t &opt = all_options.at(i);
        opt.assign_key_to_string(this->source, &name);
        // We merely invoke operator[]; this will do the insertion with a default value if necessary.
        // Note that this is somewhat nasty because it may unnecessarily copy the key. We might use a find() beforehand to save memory
        arg_t *arg = &result[name];
        
        if (! opt.default_value_range.empty() && arg->values.empty()) {
            // Apply the default value
            arg->values.push_back(string_t(this->source, opt.default_value_range.start, opt.default_value_range.length));
        }
    }
    
    // Fill in variables
    for (size_t i=0; i < all_variables.size(); i++) {
        const range_t &var_range = all_variables.at(i);
        name.assign(this->source, var_range.start, var_range.length);
        // As above, we merely invoke operator[]
        result[name];
    }
    
    return result;
}

/* Matches argv */
option_map_t match_argv(const string_list_t &argv, const positional_argument_list_t &positionals, const resolved_option_list_t &resolved_options, const option_list_t &shortcut_options, const option_list_t &all_options, const range_list_t &all_variables, const usage_t &tree, index_list_t *out_unused_arguments) {
    match_context_t ctx(positionals, resolved_options, shortcut_options, argv);
    match_state_t init_state;
    match_state_list_t result = match(tree, &init_state, &ctx);

    bool log_stuff = false;
    if (log_stuff) {
        fprintf(stderr, "Matched %lu way(s)\n", result.size());
        for (size_t i=0; i < result.size(); i++) {
            const match_state_t &state = result.at(i);
            bool is_incomplete = ! ctx.unused_arguments(&state, this->source).empty();
            std::cerr <<  "Result " << i << (is_incomplete ? " (INCOMPLETE)" : "") << ":\n";
            for (typename option_map_t::const_iterator iter = state.option_map.begin(); iter != state.option_map.end(); ++iter) {
                const string_t &name = iter->first;
                const arg_t &arg = iter->second;
                fprintf(stderr, "\t%ls: ", widen(name).c_str());
                for (size_t j=0; j < arg.values.size(); j++) {
                    if (j > 0) {
                        fprintf(stderr, ", ");
                    }
                    fprintf(stderr, "%ls", widen(arg.values.at(j)).c_str());
                }
                std::cerr << '\n';
            }
        }
    }
    
    // Return the one with the fewest unused arguments
    size_t best_state_idx = npos;
    index_list_t best_unused_args;
    for (size_t i=0; i < result.size(); i++) {
        const match_state_t &state = result.at(i);
        index_list_t unused_args = ctx.unused_arguments(&state, this->source);
        size_t unused_arg_count = unused_args.size();
        if (i == 0 || unused_arg_count < best_unused_args.size()) {
            best_state_idx = i;
            best_unused_args.swap(unused_args);
            // If we got zero, we're done
            if (unused_arg_count == 0) {
                break;
            }
        }
    }
    
    // Now return the winning state and its unused arguments
    if (best_state_idx != npos) {
        // We got a best state
        if (out_unused_arguments != NULL) {
            out_unused_arguments->swap(best_unused_args);
        }
        return finalize_option_map(result.at(best_state_idx).option_map, all_options, all_variables);
    } else {
        // No states. Every argument is unused.
        if (out_unused_arguments != NULL) {
            out_unused_arguments->clear();
            for (size_t i=0; i < argv.size(); i++) {
                out_unused_arguments->push_back(i);
            }
        }
        return finalize_option_map(option_map_t(), all_options, all_variables);
    }
}

option_map_t best_assignment(const string_list_t &argv, index_list_t *out_unused_arguments) {
    bool log_stuff = false;
    
    const range_list_t usage_ranges = this->source_ranges_for_section("Usage:");
    if (usage_ranges.empty()) {
        append_error(&this->errors, npos, "Unable to find Usage: section");
        return option_map_t();
    } else if (usage_ranges.size() > 1) {
        append_error(&this->errors, npos, "More than one Usage: section");
        return option_map_t();
    }
    
    parse_context_t ctx(this->source, usage_ranges.at(0));
    usage_t *tree = usage_t::parse(&ctx);
    
    if (! tree) {
        fprintf(stderr, "failed to parse!\n");
        return option_map_t();
    }

    // Extract options and variables from the usage sections
    option_list_t usage_options;
    range_list_t all_variables;
    this->collect_options_and_variables(*tree, &usage_options, &all_variables);

    // Extract options from the options section
    option_list_t shortcut_options = this->parse_options_spec(&this->errors);
    this->uniqueize_options(&shortcut_options, &this->errors);

    // Combine these into a single list
    option_list_t all_options;
    all_options.reserve(usage_options.size() + shortcut_options.size());
    all_options.insert(all_options.end(), usage_options.begin(), usage_options.end());
    all_options.insert(all_options.end(), shortcut_options.begin(), shortcut_options.end());
    this->uniqueize_options(&all_options, &this->errors);

    // Dump them
    if (log_stuff) {
        for (size_t i=0; i < all_options.size(); i++) {
            fprintf(stderr, "Option %lu: %ls\n", i, widen(all_options.at(i).describe(this->source)).c_str());
        }
    }

    if (log_stuff) {
        std::cerr << node_dumper_t::dump_tree(*tree, this->source) << "\n";
    }

    positional_argument_list_t positionals;
    resolved_option_list_t resolved_options;
    this->separate_argv_into_options_and_positionals(argv, all_options, &positionals, &resolved_options);

    if (log_stuff) {
        for (size_t i=0; i < positionals.size(); i++) {
            size_t arg_idx = positionals.at(i).idx_in_argv;
            fprintf(stderr, "positional %lu: %ls\n", i, widen(argv.at(arg_idx)).c_str());
        }
    }

    for (size_t i=0; i < resolved_options.size(); i++) {
        resolved_option_t &opt = resolved_options.at(i);
        range_t range = opt.option.name;
        const string_t name = string_t(this->source, range.start, range.length);
        if (log_stuff) {
            std::wstring value_str;
            if (opt.value_idx_in_argv != npos) {
                const range_t &value_range = opt.value_range_in_arg;
                value_str = widen(string_t(argv.at(opt.value_idx_in_argv), value_range.start, value_range.length));
            }
            fprintf(stderr, "Resolved %lu: %ls (%ls) <%lu, %lu>\n", i, widen(name).c_str(), value_str.c_str(), opt.option.name.start, opt.option.name.length);
        }
    }

    option_map_t result = this->match_argv(argv, positionals, resolved_options, shortcut_options, all_options, all_variables, *tree, out_unused_arguments);
    delete tree;
    return result;
}

#if SIMPLE_TEST
static int simple_test() {
    bool log_stuff = true;
    const std::string usage =
    "usage: prog (<NAME> | --foo <NAME>)\n"
    "options: --foo \n"
    ;
    
    docopt_impl<std::string> impl(usage, flags_default);
    
    const range_list_t usage_ranges = impl.source_ranges_for_section("Usage:");
    docopt_impl<std::string>::parse_context_t ctx(usage, usage_ranges.at(0));
    docopt_impl<std::string>::usage_t *tree = usage_t::parse(&ctx);
    
    if (tree == NULL) {
        fprintf(stderr, "failed to parse usage\n");
        return EXIT_FAILURE;
    }
    
    
    // Extract options from the usage sections
    option_list_t usage_options;
    range_list_t all_variables;
    impl.collect_options_and_variables(*tree, &usage_options, &all_variables);
    
    // Extract options from the options section
    option_list_t shortcut_options = impl.parse_options_spec(&impl.errors);
    impl.uniqueize_options(&shortcut_options, &impl.errors);
    
    // Combine these into a single list
    option_list_t all_options;
    all_options.reserve(usage_options.size() + shortcut_options.size());
    all_options.insert(all_options.end(), usage_options.begin(), usage_options.end());
    all_options.insert(all_options.end(), shortcut_options.begin(), shortcut_options.end());
    impl.uniqueize_options(&all_options, &impl.errors);
    
    // Dump them
    if (log_stuff) {
        for (size_t i=0; i < all_options.size(); i++) {
            fprintf(stderr, "Option %lu: %s\n", i, all_options.at(i).describe(usage).c_str());
        }
    }
    
    if (log_stuff) {
        std::string dumped = node_dumper_t::dump_tree(*tree, usage);
        fprintf(stderr, "%s\n", dumped.c_str());
    }
    
    std::vector<std::string> argv;
    argv.push_back("prog");
    argv.push_back("--foo");
    argv.push_back("10");

    
    docopt_impl<std::string>::positional_argument_list_t positionals;
    docopt_impl<std::string>::resolved_option_list_t resolved_options;
    impl.separate_argv_into_options_and_positionals(argv, all_options, &positionals, &resolved_options);
    
    for (size_t i=0; i < positionals.size(); i++) {
        fprintf(stderr, "positional %lu: %s\n", i, argv.at(positionals.at(i).idx_in_argv).c_str());
    }
    
    for (size_t i=0; i < resolved_options.size(); i++) {
        const docopt_impl<std::string>::resolved_option_t &opt = resolved_options.at(i);
        range_t range = opt.option.name;
        const std::string name = std::string(usage, range.start, range.length);
        //fprintf(stderr, "Resolved %lu: %s (%s) <%lu, %lu>\n", i, name.c_str(), opt.value.c_str_or_empty(), opt.option.name.start, opt.option.name.length);
    }
    
    index_list_t unused_arguments;
    impl.match_argv(argv, positionals, resolved_options, shortcut_options, all_options, all_variables, *tree, &unused_arguments);
    
    for (size_t i=0; i < unused_arguments.size(); i++) {
        size_t arg_idx = unused_arguments.at(i);
        fprintf(stderr, "Unused argument: <%s>\n", argv.at(arg_idx).c_str());
    }
    
    delete tree;
    return 0;
}
#endif

// close the class
CLOSE_DOCOPT_IMPL;

std::map<std::string, argument_t> docopt_parse(const std::string &usage_doc, const std::vector<std::string> &argv, parse_flags_t flags, index_list_t *out_unused_arguments) {
    docopt_impl<std::string> impl(usage_doc, flags);
    return impl.best_assignment(argv, out_unused_arguments);
}

std::map<std::wstring, wargument_t> docopt_wparse(const std::wstring &usage_doc, const std::vector<std::wstring> &argv, parse_flags_t flags, index_list_t *out_unused_arguments) {
    docopt_impl<std::wstring> impl(usage_doc, flags);
    return impl.best_assignment(argv, out_unused_arguments);
}

// close the namespace
CLOSE_DOCOPT_IMPL

#if SIMPLE_TEST
int main(void) {
    using namespace docopt_fish;
    docopt_impl<std::string>::simple_test();
    
}
#endif
