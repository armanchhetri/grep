[![progress-banner](https://backend.codecrafters.io/progress/grep/4731efce-f4c5-46d8-8fa7-67ced23499e3)](https://app.codecrafters.io/users/codecrafters-bot?r=2qF)

This is a solution to the ["Build Your Own grep" Challenge](https://app.codecrafters.io/courses/grep/overview) in Rust. It handles
a lot of pattern presented in the challenge including capture groups, wildcards, anchors, character groups(\d, \w) etc.

Run Tests:
```sh
make test
```

Run program:
```sh
chmod +x your_program.sh
echo -e "grep 101 is doing grep 101 times, and again grep 101 times\nabc-def is abc-def, not efg, abc, or def" | ./your_program.sh -E '(([abc]+)-([def]+)) is \1, not ([^xyz]+), \2, or \3'

```
Output:
```sh
abc-def is abc-def, not efg, abc, or def
```