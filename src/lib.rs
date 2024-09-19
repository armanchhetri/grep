use std::{borrow::BorrowMut, iter::Peekable};

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
    return match_line(&mut line_it, pattern);
}

fn match_line<'a, I>(mut line_it: &mut Peekable<I>, pattern: &str) -> bool
where
    I: Iterator<Item = &'a char>,
    I: Clone,
{
    let mut pattern_it = pattern.chars().peekable();
    let mut first_match = true;
    // let mut exact_match = false;
    let mut memory: Option<char> = None;
    while let Some(p) = pattern_it.next() {
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

                while let Some(c) = line_it.next() {
                    if (group_chars.contains(*c) || !first_match) {
                        matches = true;
                        break;
                    }
                }
                first_match = false;
                if neg {
                    matches = !matches
                }
                matches
            }
            '\\' => {
                let mut matches = false;
                if let Some(group) = pattern_it.next() {
                    while let Some(c) = line_it.next() {
                        matches = match group {
                            'd' => c.is_ascii_digit(),

                            'w' => c.is_alphabetic(),
                            _ => false,
                        };
                        if matches || !first_match {
                            break;
                        }
                    }
                    first_match = false;
                    matches
                } else {
                    false
                }
            }
            '(' => {
                first_match = false;
                let sub_patterns = get_all_sub_patterns(&mut pattern_it);
                let mut matches = false;
                for pattern in sub_patterns {
                    let mut copied_line_it = line_it.clone();
                    // let mut line_it_copied = copied.iter().peekable();

                    if match_line(&mut copied_line_it, &pattern) {
                        // this is to read in original iterator
                        match_line(line_it, &pattern);
                        matches = true;
                        break;
                    }
                }
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
                line_it.next();
                true
            }
            '+' => {
                if first_match {
                    false
                } else {
                    first_match = false;
                    loop {
                        if let Some(c) = line_it.peek() {
                            if let Some(m) = memory {
                                if **c == m {
                                    let _ = line_it.next();
                                } else {
                                    break;
                                }
                            }
                        }
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
                        line_it.next();
                        break;
                    }
                    if let Some(question) = pattern_it.peek() {
                        if *question == '?' {
                            pattern_it.next();
                            consume_one_or_more(&mut line_it, other);
                            matches = true;
                            break;
                        }
                    }
                    if !first_match {
                        line_it.next();
                        matches = false;
                        break;
                    }
                    line_it.next();
                }

                if let Some(question) = pattern_it.peek() {
                    if *question == '?' {
                        pattern_it.next();
                        consume_one_or_more(&mut line_it, other);
                        matches = true;
                    }
                }
                first_match = false;
                matches
            }
        };
        memory = Some(p);
        if !matches {
            return matches;
        }
    }
    return true;
}

// expected input
// abc|cde|efg)
fn get_all_sub_patterns<I>(pattern_it: &mut I) -> Vec<String>
where
    I: Iterator<Item = char>,
{
    let mut sub_patterns = Vec::new();

    let mut acc = String::new();
    while let Some(c) = pattern_it.next() {
        if c == ')' {
            if !acc.is_empty() {
                sub_patterns.push(acc);
            }
            break;
        }
        if c == '|' && !acc.is_empty() {
            sub_patterns.push(acc);
            acc = String::new();
            continue;
        }
        acc.push(c);
    }
    sub_patterns
}

fn consume_one_or_more<'a, I>(it: &mut Peekable<I>, c: char)
where
    I: Iterator<Item = &'a char>,
{
    while let Some(x) = it.peek() {
        if **x != c {
            break;
        }
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
Full fathom 5 thq fathe lie
upper hand has lower brises
Follow it till 6 pm today
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
    fn zero_or_more() {
        let query = "ca?t";
        let content = "\
caat
act
cat
";
        assert_eq!(vec!["caat", "act", "cat"], search(content, query));
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
}
