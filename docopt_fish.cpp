#include "docopt_fish.h"
#include <memory>
#include <assert.h>
#include <cstring>
#include <cstdint>

using namespace docopt_fish;

using std::auto_ptr;

static const size_t npos = (size_t)(-1);

/* Overloads */
static void assign_narrow_string_to_string(const char *s, std::string *result) {
    *result = s;
}

static void assign_narrow_string_to_string(const char *s, std::wstring *result) {
    size_t len = std::strlen(s);
    for (size_t i=0; i < len; i++) {
        char c = s[i];
        assert(c <= 127); //ASCII only
        result->push_back(wchar_t(c));
    }
}

/* Class representing a range of a string */
struct range_t {
    size_t start;
    size_t length;
    range_t() : start(0), length(0)
    {}
    range_t(size_t s, size_t l) : start(s), length(l)
    {}
    
    size_t end() const {
        size_t result = start + length;
        assert(result >= start); //don't overflow
        return result;
    }
};
typedef std::vector<range_t> range_list_t;

/* A token is just a range of some string, with a type */
struct token_t {
    enum type_t {
        none = '_',
        open_paren = '(',
        close_paren = ')',
        open_square = '[',
        close_square = ']',
        ellipsis = '.', // represents three dots ...
        bar = '|',
        word='X' // represents a word, possibly with angle brackets <foo>
    };
    
    range_t range;
    type_t type;
    
    /* Constructor */
    token_t(size_t a, size_t b, char type_char) : range(a, b)
    {
        assert(strchr("()[]|.X", type_char));
        this->type = static_cast<type_t>(type_char);
    }
    
    token_t() : range(0, 0), type(none)
    {}
    
    bool empty() const {
        return range.length == 0;
    }
    
    // For debugging
    const char *name() const {
        switch (this->type) {
            case none: return "<NONE>";
            case open_paren: return "(";
            case close_paren: return ")";
            case open_square: return "[";
            case close_square: return "]";
            case bar: return "|";
            case word: return "<word>";
            default: return NULL;
        }
    }
};
typedef std::vector<token_t> token_list_t;


/* Hide open and close brackets to avoid an annoying leading indent inside our class */
#define OPEN_DOCOPT_IMPL {
#define CLOSE_DOCOPT_IMPL }


#pragma mark -
#pragma mark Usage Grammar
#pragma mark -

/* Usage grammar:
 
    usage = WORD expression_list

    expression_list = expression opt_expression_list
 
    opt_expression_list = <empty> |
                          expression_list
 
    expression = or_clause
 
    or_clause = simple_clause or_continuation
 
    or_continuation = <empty> |
                      VERT_BAR or_clause
 
    simple_clause = WORD opt_ellipsis
                    OPEN_PAREN expression_list CLOSE_PAREN opt_ellipsis |
                    OPEN_SQUARE expression_list CLOSE_SQUARE opt_ellipsis
 
    opt_ellipsis = <empty> |
                   ELLIPSIS
*/
class usage_t;
class expression_list_t;
class opt_expression_list_t;
class expression_t;
class or_clause_t;
class or_continuation_t;
class simple_clause_t;
class opt_ellipsis_t;

/* Context passed around in our recursive descent parser */
struct parse_context_t {
    const token_list_t tokens;
    range_t remaining_range;
    
    parse_context_t(token_list_t &lst) : tokens(lst), remaining_range(0, lst.size())
    {}

    /* Returns true if there are no more next tokens */
    bool is_at_end() const {
        return remaining_range.length == 0;
    }
    
    /* Returns true if there is a next token, and it has the given type */
    bool next_token_has_type(token_t::type_t t) const {
        bool result = false;
        if (! this->is_at_end()) {
            result = tokens.at(remaining_range.start).type == t;
        }
        return result;
    }
    
    token_t::type_t next_token_type() const {
        if (this->is_at_end()) {
            return token_t::none;
        } else {
            return tokens.at(remaining_range.start).type;
        }
    }

    /* Returns the next token, decreasing the remaining range. Requires that there be a next token */
    token_t acquire_token() {
        assert(! this->is_at_end());
        size_t idx = remaining_range.start;
        remaining_range.start += 1;
        remaining_range.length -= 1;
        return tokens.at(idx);
    }
};


/* Helpers to parse when the productions are fixed. */
template<typename PARENT, typename CHILD>
PARENT *parse_1(parse_context_t *ctx) {
    PARENT *result = NULL;
    auto_ptr<CHILD> child(CHILD::parse(ctx));
    if (child.get() != NULL) {
        result = new PARENT(child);
    }
    return result;
}

template<typename PARENT, typename CHILD1, typename CHILD2>
PARENT *parse_2(parse_context_t *ctx) {
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
PARENT *parse_1_or_empty(parse_context_t *ctx) {
    PARENT *result = NULL;
    auto_ptr<CHILD> child(CHILD::parse(ctx));
    if (child.get()) {
        result = new PARENT(child);
    } else {
        result = new PARENT();
    }
    return result;
}

/* Little helper class for dumping */
class node_dumper_t {
    std::string text;
    unsigned int depth;
    
    // Instances of this class are expected to be quite transient. src is unowned.
    const std::string *src;
    
    std::vector<std::string> lines;
    
    template<typename NODE_TYPE>
    void dump_tree_internal(const NODE_TYPE& node) {
        std::string result(2 * depth, ' ');
        result.append(node.name());
        lines.push_back(result);
        unsigned unindent = node.unindent();
        depth = depth + 1 - unindent;
        node.dump_children(this);
        depth = depth - 1 + unindent;
    }
    
public:
    template<typename T1>
    void dump(const T1 &t1) {
        /* Here t1 is expected to be auto_ptr<node_type> */
        if (t1.get() != NULL) {
            dump_tree_internal(*t1);
        }
    }
    
    void dump(const token_t &t1) {
        if (t1.type != token_t::none) {
            std::string result(2 * depth, ' ');
            char buff[32];
            if (t1.type == token_t::word && src != NULL) {
                const std::string word(*src, t1.range.start, t1.range.length);
                snprintf(buff, sizeof buff, "'%s' {%lu-%lu}", word.c_str(), t1.range.start, t1.range.length);
            } else {
                snprintf(buff, sizeof buff, "'%s' {%lu-%lu}", t1.name(), t1.range.start, t1.range.length);
            }
            result.append(buff);
            lines.push_back(result);
        }
    }

    template<typename T1, typename T2>
    void dump(const T1 & t1, const T2 & t2) {
        dump(t1);
        dump(t2);
    }
    
    template<typename T1, typename T2, typename T3, typename T4, typename T5>
    void dump(const T1 & t1, const T2 & t2, const T3 & t3, const T4 &t4, const T5 &t5) {
        dump(t1);
        dump(t2);
        dump(t3);
        dump(t4);
        dump(t5);
    }
    
    template<typename NODE_TYPE>
    static std::string dump_tree(const NODE_TYPE &node, const std::string &src) {
        node_dumper_t dumper;
        dumper.src = &src;
        dumper.dump_tree_internal(node);
        std::string result;
        for (size_t i=0; i < dumper.lines.size(); i++) {
            result.append(dumper.lines.at(i));
            result.push_back('\n');
        }
        dumper.src = NULL;
        return result;
    }
};

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
    static expression_list_t *parse(struct parse_context_t *ctx) {  return parse_2<expression_list_t, expression_t, opt_expression_list_t>(ctx); }
    std::string name() const { return "expression_list"; }
    
    void dump_children(node_dumper_t *d) const {
        d->dump(expression, opt_expression_list);
    }
    
    /* Don't indent children of opt_expression_list to keep the list looking flatter */
    unsigned unindent() const { return 1; }
};


struct usage_t : public base_t {
    token_t prog_name;
    auto_ptr<expression_list_t> expression_list;

    // usage = expression_list
    usage_t(token_t t, auto_ptr<expression_list_t> c) : prog_name(t), expression_list(c) {}
    
    static usage_t *parse(struct parse_context_t *ctx) {
        usage_t *result = NULL;
        // TODO: generate error for missing word name
        if (ctx->next_token_has_type(token_t::word)) {
            token_t p = ctx->acquire_token();
            auto_ptr<expression_list_t> el(expression_list_t::parse(ctx));
            if (el.get()) {
                result = new usage_t(p, el);
            }
        }
        return result;
    }
    
    std::string name() const { return "usage"; }
    void dump_children(node_dumper_t *d) const {
        d->dump(prog_name, expression_list);
    }
    
};

struct opt_expression_list_t : public base_t {
    auto_ptr<expression_list_t> expression_list;
    
    // opt_expression_list = empty
    opt_expression_list_t() {}
    
    // opt_expression_list = expression_list
    opt_expression_list_t(auto_ptr<expression_list_t> c) : base_t(1), expression_list(c) {}
    static opt_expression_list_t *parse(struct parse_context_t *ctx) { return parse_1_or_empty<opt_expression_list_t, expression_list_t>(ctx); }
    std::string name() const { return "opt_expression_list"; }
    void dump_children(node_dumper_t *d) const {
        d->dump(expression_list);
    }
    unsigned unindent() const { return 1; }
};

struct expression_t : public base_t {
    auto_ptr<or_clause_t> or_clause;
    
    // expression = or_clause
    expression_t(auto_ptr<or_clause_t> c) : or_clause(c) {}
    static expression_t *parse(struct parse_context_t *ctx) { return parse_1<expression_t, or_clause_t>(ctx); }
    std::string name() const { return "expression"; }
    void dump_children(node_dumper_t *d) const {
        d->dump(or_clause);
    }
};

struct or_clause_t : public base_t {
    auto_ptr<simple_clause_t> clause;
    auto_ptr<or_continuation_t> or_continuation;
    
    //or_clause_t = simple_clause or_continuation
    or_clause_t(auto_ptr<simple_clause_t> c1, auto_ptr<or_continuation_t> c2) : clause(c1), or_continuation(c2) {}
    static or_clause_t *parse(struct parse_context_t *ctx) { return parse_2<or_clause_t, simple_clause_t, or_continuation_t>(ctx); }
    std::string name() const { return "or_clause"; }
    void dump_children(node_dumper_t *d) const {
        d->dump(clause, or_continuation);
    }
};

struct or_continuation_t : public base_t {
    token_t vertical_bar;
    auto_ptr<or_clause_t> or_clause;
    
    // or_continuation = <empty>
    or_continuation_t() {}
    
    // or_continuation =  VERT_BAR or_clause
    or_continuation_t(token_t b, auto_ptr<or_clause_t> c) : base_t(1), vertical_bar(b), or_clause(c) {}
    
    static or_continuation_t *parse(struct parse_context_t *ctx) {
        or_continuation_t *result = NULL;
        if (ctx->next_token_has_type(token_t::bar)) {
            token_t token = ctx->acquire_token();
            auto_ptr<or_clause_t> c(or_clause_t::parse(ctx));
            if (c.get()) {
                result = new or_continuation_t(token, c);
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
    void dump_children(node_dumper_t *d) const {
        d->dump(vertical_bar, or_clause);
    }
};

struct opt_ellipsis_t : public base_t {
    token_t ellipsis;
    
    // opt_ellipsis = <empty>
    opt_ellipsis_t() {}
    
    // opt_ellipsis = ELLIPSIS
    opt_ellipsis_t(token_t t) : base_t(1), ellipsis(t) {}
    
    static opt_ellipsis_t *parse(struct parse_context_t *ctx) {
        opt_ellipsis_t *result = NULL;
        if (ctx->next_token_has_type(token_t::ellipsis)) {
            result = new opt_ellipsis_t(ctx->acquire_token());
        } else {
            result = new opt_ellipsis_t();
        }
        return result;
    }
    
    std::string name() const { return "opt_ellipsis"; }
    void dump_children(node_dumper_t *d) const {
        d->dump(ellipsis);
    }
};

struct simple_clause_t : public base_t {
    // production 0
    token_t word;
    
    // Collapsed for productions 1 and 2
    token_t open_token;
    auto_ptr<expression_list_t> expression_list;
    token_t close_token;
    
    // All ellipsis are collapsed into this
    auto_ptr<opt_ellipsis_t> opt_ellipsis;
    
    //simple_clause = WORD opt_ellipsis
    simple_clause_t(token_t w, auto_ptr<opt_ellipsis_t> e) : base_t(0), word(w), opt_ellipsis(e) {}

    //simple_clause = OPEN_PAREN expression_list CLOSE_PAREN opt_ellipsis |
    //simple_clause = OPEN_SQUARE expression_list CLOSE_SQUARE opt_ellipsis
    simple_clause_t(token_t a, auto_ptr<expression_list_t> el, token_t b, auto_ptr<opt_ellipsis_t> e)
        : base_t(a.type == token_t::open_paren ? 1  : 2), open_token(a), expression_list(el), close_token(b), opt_ellipsis(e)
    {}

    static simple_clause_t *parse(struct parse_context_t * ctx) {
        simple_clause_t *result = NULL;
        if (ctx->is_at_end()) {
            fprintf(stderr, "At end\n");
            return NULL;
        }
        
        switch (ctx->next_token_type()) {
            case token_t::open_paren:
            case token_t::open_square:
            {
                token_t init_token = ctx->acquire_token();
                auto_ptr<expression_list_t> contents(expression_list_t::parse(ctx));
                if (contents.get()) {
                    bool is_paren = (init_token.type == token_t::open_paren);
                    token_t::type_t close_type = (is_paren ? token_t::close_paren : token_t::close_square);
                    if (ctx->next_token_has_type(close_type)) {
                        token_t close_token = ctx->acquire_token();
                        auto_ptr<opt_ellipsis_t> ellipsis(opt_ellipsis_t::parse(ctx));
                        assert(ellipsis.get() != NULL); // should never fail
                        result = new simple_clause_t(init_token, contents, close_token, ellipsis);
                    } else {
                        // TODO: generate error of unclosed paren
                    }
                }
                break;
            }
            
            case token_t::close_paren:
            case token_t::close_square:
                result = NULL;
                break;
                
            case token_t::ellipsis:
            case token_t::bar:
                // Indicates leading ellipsis / bar
                // TODO: generate error
                break;
                
            case token_t::word:
            {
                token_t init_token = ctx->acquire_token();
                auto_ptr<opt_ellipsis_t> ellipsis(opt_ellipsis_t::parse(ctx));
                assert(ellipsis.get() != NULL); // should never fail
                result = new simple_clause_t(init_token, ellipsis);
                break;
            }
                
            case token_t::none:
                assert(0 && "none-type token returned from next_token_type when not at end of token stream");
                break;
        }
        return result;
    }
    
    std::string name() const { return "simple_clause"; }
    void dump_children(node_dumper_t *d) const {
        d->dump(word, open_token, expression_list, close_token, opt_ellipsis);
    }

};

#pragma mark -


/* Wrapper template class that takes either a string or wstring as string_t */
template<typename string_t>
class docopt_impl OPEN_DOCOPT_IMPL

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
docopt_impl(const string_t &s) : source(s)
{}

/* Helper function to make a string (narrow or wide) from a C string */
static string_t string_from_narrow_string(const char *s) {
    string_t result;
    assign_narrow_string_to_string(s, &result);
    return result;
}

/* List of errors */
error_list_t errors;
void append_error(error_list_t *errors, size_t where, const char *txt) {
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
typedef typename string_t::value_type char_t;

/* Immutable class representing an optional string */
class optional_string_t {
    const bool imissing;
    const string_t ivalue;
    
    optional_string_t() : imissing(true) {}
    optional_string_t(const std::string &s) : imissing(false), ivalue(s) {}
    
    bool missing() const {
        return imissing;
    }
    
    const string_t &value() {
        assert(! this->missing);
        return ivalue;
    }
};

/* Helper function to efficiently iterate over lines of a string 'source'. On input, line_end should be initialized to the start point for the iteration. On return, line_start will point at the next line, and line_end will point just after the trailing newline, or possibly at source.size(). The length of the line is line_end - line_start (and is guaranteed to be positive). Returns true if a line was returned, false if we reached the end. */
static bool get_next_line(const string_t &str, size_t *line_start, size_t *line_end)
{
    bool success = false;
    if (*line_end < str.size())
    {
        // Start at the end of the last line, or zero if this is the first call
        *line_start = *line_end;
        size_t newline = str.find('\n', *line_start);
        if (newline == std::string::npos) {
            // Point just after the last character
            *line_end = str.size();
        } else {
            // Point just after the newline
            *line_end = newline + 1;
        }
        // Empty lines are impossible
        assert(*line_end > *line_start);
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

/* Given a name like 'options:' and the usage text (as source), return a list of substrings of the
 source text that match the usage. The Python docopt is quite permissive, and is prone to false
 positive. For example, if the word "options:" appears anywhere in a line, it may be picked up. We
 wish to be more conservative. We model the source text as a list of lines, separated into sections.
 A section has a header like 'foo options:' that must not be indented (no leading whitespace) and must
 have a colon. All lines after the header until the next header are part of that section.
 */
string_list_t parse_section(const char *name) {
    string_list_t sections;
    
    bool in_desired_section = false;
    size_t line_start = 0, line_end = 0;
    while (get_next_line(source, &line_start, &line_end)) {
        // It's a header line if the first character is not whitespace
        if (! isspace(source.at(line_start))) {
            // Check to see if the name is found before the first colon
            // Note that if name is not found at all, name_pos will have value npos, which is huge (and therefore not smaller than line_end)
            size_t name_pos = find_case_insensitive(source, name, line_start);
            in_desired_section = (name_pos < line_end && name_pos < source.find(':', line_start));
            if (in_desired_section) {
                // This line is the start of a section we want.
                // Append a blank line to our result vector to hold this section.
                sections.push_back(std::string());
            }
        }
        
        if (in_desired_section) {
            // We're in the section we want. Append the line to the current section.
            // Note this line may be its header.
            // Also note that result must be nonempty, because we always append to result at the point that in_desired_section is set to true
            sections.back().append(source, line_start, line_end - line_start);
        }
        
        // Update our next line to start just past the newline (or past the end of the source)
        line_start = line_end;
    }
    
    return sections;
}

/* Class representing options */
class option_t {
    /* Name, like 'speed' */
    string_t name;
    
    /* Default value, like '10'.  */
    optional_string_t default_value;
    
    /* How many dashes appeared, at least 1 */
    unsigned dash_count;
};

/* Returns true if the given token matches the given narrow string, up to len characters */
bool token_substr_equals(const token_t &tok, const char *str, size_t len) {
    assert(tok.range.end() < this->source.length());
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

/* Tokenize a usage specification from 'source' */
token_list_t tokenize_usage(size_t start, size_t end) {
    /* The following are tokens:
         [, ], (, ), |, ..., strings_without_spaces, <stuff in brackets>
         A string_without_spaces abutting <stuff_in_brackets> like so:
            foo<bar baz>
         counts as only a single token.
    */
    assert(end >= start);
    token_list_t result;
    bool within_brackets = false;
    size_t bracket_start = npos;
    size_t word_start = npos;
    
    const token_t::type_t word_type = token_t::word;
    
    const char_t *sc = source.c_str() + start;
    const size_t size = end - start;
    
    token_list_t tokens;
    for (size_t i=0; i < size;)
    {
        const char_t c = sc[i];
        if (! within_brackets)
        {
            if (contains("()[]|", c)) {
                // Some sort of parenthesis
                if (word_start != npos) {
                    tokens.push_back(token_t(word_start, i - word_start, word_type));
                    word_start = npos;
                }
                tokens.push_back(token_t(i, 1, c));
                i += 1;
            } else if (contains(" \t\n", c)) {
                // Whitespace
                if (word_start != npos) {
                    tokens.push_back(token_t(word_start, i - word_start, word_type));
                    word_start = npos;
                }
                i += 1;
            } else if (substr_equals("...", sc + i, 3)) {
                // Ellipsis
                if (word_start != npos) {
                    tokens.push_back(token_t(word_start, i - word_start, word_type));
                    word_start = npos;
                }
                tokens.push_back(token_t(i, 3, '.'));
                i += 3;
            } else {
                // Normal char, or possibly bracket
                if (word_start == npos) {
                    word_start = i;
                }
                if (c == '<') {
                    within_brackets = true;
                    bracket_start = i;
                }
                i += 1;
            }
        } else {
            // Within brackets
            if (c == '>') {
                within_brackets = false;
                bracket_start = npos;
            }
            i += 1;
        }
    }
    
    if (within_brackets) {
        append_error(&this->errors, bracket_start, "Unclosed bracket");
    }
    
    // grab trailing word
    if (word_start != npos) {
        tokens.push_back(token_t(word_start, size - word_start, word_type));
        word_start = npos;
    }
    return tokens;
}

CLOSE_DOCOPT_IMPL;

#if 0
std::map<std::string, argument_t> docopt_parse(const std::string &doc, const std::vector<std::string> &argv) {
    
}

std::map<std::wstring, wargument_t> docopt_wparse(const std::wstring &doc, const std::vector<std::wstring> &argv) {
    
}
#endif

int main(void) {
    using namespace docopt_fish;
    const std::string usage = "naval_fate ship <name> move <x> <y> [--speed=<kn>]";
    docopt_impl<std::string> impl(usage);
    token_list_t tokens = impl.tokenize_usage(0, usage.size());
    fprintf(stderr, "%s\n", usage.c_str());
    for (size_t i=0; i < tokens.size(); i++) {
        const token_t &tok = tokens.at(i);
        fprintf(stderr, "%lu: '%c' [%lu, %lu]\n", i, (char)tok.type, tok.range.start, tok.range.length);
    }
    
    parse_context_t ctx(tokens);
    usage_t *tree = usage_t::parse(&ctx);
    
    std::string dumped = node_dumper_t::dump_tree(*tree, usage);
    fprintf(stderr, "%s\n", dumped.c_str());
    
    delete tree;
    
    return 0;
}
