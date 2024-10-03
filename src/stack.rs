use std::collections::HashMap;

pub struct Stack<T> {
    memory: Vec<T>,
}

impl<T> Stack<T> {
    pub fn new() -> Stack<T> {
        return Stack { memory: Vec::new() };
    }
    pub fn push(&mut self, c: T) {
        self.memory.push(c);
    }

    pub fn pop(&mut self) -> Option<T> {
        return self.memory.pop();
    }
}

fn recurse(arr: &mut HashMap<usize, String>, n: usize) {
    if n == 0 {
        return;
    }
    println!("{:?}", arr.get(&n));

    recurse(arr, n - 1);

    arr.insert(n, String::from(format!("Hello {}", n)));
    recurse(arr, n - 1);
    println!("{:?}", arr.get(&n));
}

mod tests {
    use super::*;

    #[test]
    fn test_recurse() {
        let mut v: HashMap<usize, String> = HashMap::new();
        recurse(&mut v, 5);
        println!("{:?}", v);
    }

    #[test]
    fn push() {
        let mut stack = Stack::<char>::new();
        stack.push('a');
    }
    #[test]
    fn pop() {
        let mut stack = Stack::<char>::new();
        stack.push('a');
        stack.push('b');
        let popped = stack.pop().unwrap();
        assert_eq!(popped, 'b');
        let popped = stack.pop().unwrap();
        assert_eq!(popped, 'a');
    }
}
