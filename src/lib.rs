pub mod stack;

use std::{collections::HashMap, iter::Peekable};

pub fn search<'a>(content: &'a str, pattern: &str) -> Vec<&'a str> {
    // let regex = Regex::compile(pattern);

    // println!("{:?}", regex);
    // println!("{content}, {pattern}");
    let filtered: Vec<&str> = content
        .lines()
        .filter(|line| match_it(line, pattern))
        .collect();
    for line in &filtered {
        println!("{line}");
    }
    filtered
}

fn match_it(line: &str, pattern: &str) -> bool {
    let mut line_vec: Vec<char> = line.chars().collect();
    let mut line_it = line_vec.iter().peekable();
    let mut capture_group: HashMap<i32, String> = HashMap::new();

    let matched_seq = match_line(&mut line_it, pattern, true, &mut capture_group, 0);
    return !matched_seq.is_empty();
}

fn match_line<'a, I>(
    mut line_it: &mut Peekable<I>,
    pattern: &str,
    mut first_match: bool,
    capture_group: &mut HashMap<i32, String>,
    curr_idx: i32,
) -> String
where
    I: Iterator<Item = &'a char>,
    I: Clone,
{
    let mut next_idx = curr_idx;
    // let mut copied_line_it: &mut Peekable<I>;
    let mut pattern_it = pattern.chars().peekable();
    // let mut first_match = true;
    let mut matched_seq = String::new();
    // let mut exact_match = false;
    let mut memory: Memory = Memory::None;
    let mut capture_groups: Vec<String> = Vec::new();
    while let Some(p) = pattern_it.next() {
        let mut temp_memory = Memory::None;
        let matches = match p {
            '[' => {
                let mut matches = false;
                let mut group_chars = String::new();
                let mut neg = false;
                for group_char in &mut pattern_it {
                    if group_char == '^' {
                        neg = true;
                        continue;
                    }
                    if group_char == ']' {
                        break;
                    }
                    group_chars.push(group_char);
                }

                if neg {
                    matches = true;
                    while let Some(c) = line_it.next() {
                        if group_chars.contains(*c) {
                            matches = false;
                            break;
                        }
                        if !first_match {
                            matches = true;
                            break;
                        }
                        matched_seq.push(*c);
                    }
                } else {
                    while let Some(c) = line_it.next() {
                        if group_chars.contains(*c) {
                            matched_seq.push(*c);
                            matches = true;
                            break;
                        }
                        if !first_match {
                            matches = false;
                            break;
                        }
                    }
                }

                first_match = false;
                if neg {
                    temp_memory = Memory::NegativeGroup(group_chars);
                } else {
                    temp_memory = Memory::PositiveGroup(group_chars);
                }
                matches
            }
            '\\' => {
                let mut matches = false;
                if let Some(directive) = pattern_it.next() {
                    if directive.is_numeric() {
                        let index: i32 = directive.to_digit(10).unwrap() as i32;
                        assert!(index > 0);
                        // let pattern: &String = &capture_groups[index - 1];
                        println!("index: {index}, map: {:?}", capture_group);
                        let pattern = capture_group.get(&index).unwrap().clone();
                        let matched_substring =
                            match_line(line_it, &pattern, false, capture_group, -100);

                        matched_seq.push_str(&matched_substring);
                        matches = !matched_substring.is_empty()
                    } else {
                        while let Some(c) = line_it.next() {
                            matched_seq.push(*c);
                            matches = match directive {
                                'd' => {
                                    temp_memory = Memory::Numeric;
                                    c.is_ascii_digit()
                                }

                                'w' => {
                                    temp_memory = Memory::Alphabetic;
                                    c.is_alphabetic()
                                }
                                _ => false,
                            };

                            if matches || !first_match {
                                break;
                            }
                        }
                    }

                    first_match = false;
                    matches
                } else {
                    false
                }
            }
            '(' => {
                next_idx += 1;
                let sub_patterns = get_all_sub_patterns(&mut pattern_it);

                let mut matches = false;

                if sub_patterns.len() == 1 {
                    let matched_substring = match_line(
                        &mut line_it,
                        &sub_patterns[0],
                        first_match,
                        capture_group,
                        next_idx,
                    );
                    if !matched_substring.is_empty() {
                        matched_seq.push_str(&matched_substring);
                        matches = true;
                    }
                } else {
                    for pattern in sub_patterns {
                        let mut copied_line_it = line_it.clone();

                        let matched_substring = match_line(
                            &mut copied_line_it,
                            &pattern,
                            first_match,
                            capture_group,
                            next_idx,
                        );

                        if !matched_substring.is_empty() {
                            matched_seq.push_str(&matched_substring);
                            // this is to read in original iterator, could not get around borrow checker :(
                            match_line(line_it, &pattern, first_match, capture_group, -100);
                            matches = true;
                            break;
                        }
                    }
                }

                first_match = false;
                matches
            }
            '^' => {
                first_match = false;
                true
            }
            '$' => {
                first_match = false;
                // let mut matches = true;
                line_it.next().is_none()
            }
            '.' => {
                first_match = false;
                if let Some(c) = line_it.next() {
                    matched_seq.push(*c);
                    true
                } else {
                    false
                }
            }
            '+' => {
                if first_match {
                    false
                } else {
                    first_match = false;

                    while let Some(c) = line_it.peek() {
                        let has_match = match memory {
                            Memory::Value(m) => m == **c,
                            Memory::Alphabetic => c.is_alphabetic(),
                            Memory::Numeric => c.is_numeric(),
                            Memory::PositiveGroup(ref g) => g.contains(**c),
                            Memory::NegativeGroup(ref g) => !g.contains(**c),
                            Memory::None => false,
                        };
                        if !has_match {
                            break;
                        }
                        matched_seq.push(*line_it.next().unwrap());
                    }

                    true
                }
            }
            '?' => {
                first_match = false;
                true
            }

            other => {
                let mut matches = false;
                while let Some(c) = line_it.peek() {
                    if **c == other {
                        matches = true;
                        matched_seq.push(*line_it.next().unwrap());
                        break;
                    }
                    if let Some(question) = pattern_it.peek() {
                        if *question == '?' {
                            pattern_it.next();
                            matches = true;
                            break;
                        }
                    }
                    if !first_match {
                        matched_seq.push(*line_it.next().unwrap());
                        matches = false;
                        break;
                    }
                    matched_seq.push(*line_it.next().unwrap());
                }

                if let Some(question) = pattern_it.peek() {
                    if *question == '?' {
                        pattern_it.next();
                        matches = true;
                    }
                }
                first_match = false;
                matches
            }
        };
        if temp_memory == Memory::None {
            memory = Memory::Value(p);
        } else {
            memory = temp_memory;
        }
        if !matches {
            return String::new();
        }
    }
    println!(
        "capture_group before inserting {} = {:?}",
        curr_idx, capture_group
    );
    capture_group.insert(curr_idx, matched_seq.clone());
    println!(
        "capture_group after inserting {} = {:?}",
        curr_idx, capture_group
    );

    return matched_seq;
}

#[derive(PartialEq)]
enum Memory {
    Value(char),
    PositiveGroup(String),
    NegativeGroup(String),
    Alphabetic,
    Numeric,
    None,
}

// expected input
// abc|cde|efg)
fn get_all_sub_patterns<I>(pattern_it: &mut I) -> Vec<String>
where
    I: Iterator<Item = char>,
{
    let mut bracket_stack: Vec<char> = Vec::from(['(']);

    let mut sub_patterns = Vec::new();

    let mut acc = String::new();
    while let Some(c) = pattern_it.next() {
        if c == '(' {
            bracket_stack.push(c);
        }
        if c == ')' {
            bracket_stack.pop();
            if !acc.is_empty() & bracket_stack.is_empty() {
                sub_patterns.push(acc.clone());
                break;
            }
        }
        if c == '|' && !acc.is_empty() {
            sub_patterns.push(acc.clone());
            acc = String::new();
            continue;
        }
        acc.push(c);
    }
    sub_patterns
}

fn consume_more<'a, I>(it: &mut Peekable<I>, c: char, mut times: i32)
where
    I: Iterator<Item = &'a char>,
{
    while let Some(x) = it.peek() {
        if **x != c {
            break;
        }
        if times == 0 {
            break;
        }
        times -= 1;
        it.next();
    }
}

#[derive(Debug)]
enum RegexType {
    Invalid,
    Digit,
    Word,
    Char(char),
    Literal(String),
    LiteralFree(String),
    CharGroup(String),
    NegCharGroup(String),
}
#[derive(Debug)]
struct Regex {
    tree: Vec<RegexType>,
}

impl Regex {
    fn compile(pattern: &str) -> Regex {
        let mut tree = Vec::new();
        let mut pattern_it = pattern.chars();
        while let Some(c) = pattern_it.next() {
            let reg_type = match c {
                '\\' => match pattern_it.next() {
                    Some('d') => RegexType::Digit,
                    Some('w') => RegexType::Word,
                    Some(ch) => RegexType::Char(ch),
                    None => RegexType::Invalid,
                },
                '[' => {
                    let mut char_group = String::new();
                    let (mut error, mut negative) = (false, false);

                    loop {
                        match pattern_it.next() {
                            Some('^') => negative = true,
                            Some(gc) => {
                                if gc == ']' {
                                    break;
                                }
                                char_group.push(gc);
                            }
                            None => {
                                error = true;
                                break;
                            }
                        };
                    }
                    if error {
                        RegexType::Invalid
                    } else if negative {
                        RegexType::NegCharGroup(char_group)
                    } else {
                        RegexType::CharGroup(char_group)
                    }
                }
                c => RegexType::Char(c),
            };

            tree.push(reg_type);
        }

        // convert consecutive chars to Literal type
        let mut tree_final: Vec<RegexType> = Vec::new();
        let mut literals = String::new();
        for regex_type in tree {
            match regex_type {
                RegexType::Char(c) => literals.push(c),
                rt => {
                    if !literals.is_empty() {
                        tree_final.push(RegexType::LiteralFree(literals));
                        literals = String::new();
                    }
                    tree_final.push(rt);
                }
            }
        }
        Regex { tree: tree_final }
    }

    fn execute(&self, line: &str) -> bool {
        let mut line_it = line.chars().peekable();
        for rule in &self.tree {
            if !self.engulf(rule, &mut line_it) {
                return false;
            }
        }
        true
    }

    fn engulf<I>(&self, rule: &RegexType, it: &mut Peekable<I>) -> bool
    where
        I: Iterator<Item = char>,
    {
        match rule {
            RegexType::Char(ch) => it.any(|c| c == *ch),
            RegexType::CharGroup(char_group) => it.any(|c| char_group.contains(c)),
            RegexType::NegCharGroup(char_group) => it.all(|c| !char_group.contains(c)),
            RegexType::LiteralFree(literals) => it.any(|c| literals.contains(c)),
            RegexType::Literal(literals) => {
                let mut literal_match = true;
                for l in literals.chars() {
                    if let Some(c) = it.next() {
                        if c != l {
                            literal_match = false;
                            break;
                        }
                    }
                }
                literal_match
            }
            RegexType::Digit => it.any(|c| c.is_ascii_digit()),
            RegexType::Word => it.any(|c| c.is_ascii_alphabetic()),
            _ => false,
        }
        // while let Some(c) = it.peek() {
        //     let ret_val = match rule {
        //         RegexType::Literal(x) => c == x,
        //         RegexType::Digit => c.is_ascii_digit(),
        //         RegexType::Word => c.is_ascii_alphabetic(),
        //         RegexType::CharGroup(char_group) => char_group.contains(*c),
        //         RegexType::NegCharGroup(char_group) => {
        //             let mut char_exists = false;
        //             for ch in &mut *it {
        //                 if char_group.contains(ch) {
        //                     char_exists = true;
        //                     break;
        //                 }
        //             }
        //             !char_exists
        //         }
        //         _ => false,
        //     };
        //
        //     it.next();
        //     if ret_val {
        //         return ret_val;
        //     }
        // }
        // false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_all_subpattern() {
        let pattern = "'(cat) and \\2') is the same as \\1";
        assert_eq!(
            get_all_sub_patterns(&mut pattern.chars()),
            vec!["'(cat) and \\2'"]
        );
    }

    #[test]
    fn nested_and_multiple_capture_group() {
        let query = "((\\w\\w\\w\\w) (\\d\\d\\d)) is doing \\2 \\3 times, and again \\1 times";
        let content = "\
grep 101 is doing grep 101 times, and again grep 101 times
3 red squares and 4 red circles
";
        assert_eq!(
            vec!["grep 101 is doing grep 101 times, and again grep 101 times"],
            search(content, query)
        );
    }
    #[test]
    fn multiple_capture_groups() {
        let query = "(\\d+) (\\w+) squares and \\1 \\2 circles";
        let content = "\
3 red squares and 3 red circles
3 red squares and 4 red circles
";
        assert_eq!(
            vec!["3 red squares and 3 red circles"],
            search(content, query)
        );
    }

    #[test]
    fn nested_capture_groups() {
        let query = "('(cat) and \\2') is the same as \\1";
        let content = "\
'cat and cat' is the same as 'cat and cat'
";
        assert_eq!(
            vec!["'cat and cat' is the same as 'cat and cat'"],
            search(content, query)
        );
    }

    #[test]
    fn multiple_capture_groups_with_anchors() {
        let query = "^(apple) (\\w+), \\1 and \\2$";
        let content = "\
pineapple pie, pineapple and pie
apple pie, apple and pie
";
        assert_eq!(vec!["apple pie, apple and pie"], search(content, query));
    }

    #[test]
    fn char_search() {
        let query = "I";
        let content = "\
there is someone
I don't know what to do";
        assert_eq!(vec!["I don't know what to do"], search(content, query));
    }

    #[test]
    fn digit_search() {
        let query = "\\d";
        let content = "\
Full fathom 5 thy father lie
upper hand has lower brises
Follow it till 6 pm today
            ";
        assert_eq!(
            vec!["Full fathom 5 thy father lie", "Follow it till 6 pm today"],
            search(content, query)
        )
    }
    #[test]
    fn char_group_search() {
        let query = "[rnq]";
        let content = "\
Full fathom 5 thq fathe lie
upper hand has lower brises
Follow it till 6 pm today
            ";
        assert_eq!(
            vec!["Full fathom 5 thq fathe lie", "upper hand has lower brises"],
            search(content, query)
        );
    }

    #[test]
    fn neg_char_group_search() {
        let query = "[^rnq]";
        let content = "\
Follow it till 6 pm today
Full fathom 5 thq fathe lie
upper hand has lower brises
";
        assert_eq!(vec!["Follow it till 6 pm today"], search(content, query));
    }

    #[test]
    fn combined_pattern_search() {
        let query = "\\d apple";
        let content = "\
1 apple
2 apples
3 oranges
5 app
";
        assert_eq!(vec!["1 apple", "2 apples"], search(content, query));
    }
    #[test]
    fn string_anchor_start() {
        let query = "^made";
        let content = "\
made in nepal
not made in nepal
";
        assert_eq!(vec!["made in nepal"], search(content, query));
    }
    #[test]
    fn string_achor_end() {
        let query = "made$";
        let content = "\
made in nepal
In nepal made
";
        assert_eq!(vec!["In nepal made"], search(content, query));
    }

    #[test]
    fn one_or_more() {
        let query = "ca+ts";
        let content = "\
my cats
my caaats
";
        assert_eq!(vec!["my cats", "my caaats"], search(content, query));
    }
    #[test]
    fn zero_or_one() {
        let query = "ca?t";
        let content = "\
caat
act
cat
";
        assert_eq!(vec!["act", "cat"], search(content, query));
    }
    #[test]
    fn single_wildcard() {
        let query = "d.g";
        let content = "\
dog
dig
god
";
        assert_eq!(vec!["dog", "dig"], search(content, query));
    }

    #[test]
    fn alternation() {
        let query = "(cat|dog)";
        let content = "\
dog
cat
my cat
my god
";
        assert_eq!(vec!["dog", "cat", "my cat"], search(content, query));
    }

    #[test]
    fn capture_groups() {
        let query = "(\\w+) and \\1";
        let content = "\
cat and cat
dog and dog
cat and dog
";
        assert_eq!(vec!["cat and cat", "dog and dog"], search(content, query));
    }

    #[test]
    fn negative_group_at_end() {
        let query = "([abcd]+) is \\1, not [^xyz]+";
        let content = "\
abcd is abcd, not efg
";
        assert_eq!(vec!["abcd is abcd, not efg"], search(content, query));
    }

    #[test]
    fn capture_group_with_zero_or_more() {
        let query = "once a (drea+mer), alwaysz? a \\1";
        let content = "\
once a dreaaamer, alwayszzz a dreaaamer
";
        assert!(search(content, query).is_empty());
    }
}
