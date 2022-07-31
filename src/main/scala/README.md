# JSON Parsing 

## Example 1
```
[EOF
```
parses as:

```
[]
```

## Example 2
```
{EOF
```
parses as:

```
{}
```

## Example 3
```
{}{}
```
parses as:

```
{},{}
```

## Example 4

```
{
  {},
}
```
parses as:

```
{
  {}
}
```

## Example 5
This result may not be what is desired.

```
[
  {   <-- missing closing '}'
  {}
]
```
parses as:

```
[
  {
    {}
  }
]
```

# Principles
- commas outside of quotes are optional
- any un-closed ] or } characters are automatically closed when EOF is encountered
- Stack elements are immutable?


# Parsing process

## Input 1

```
[ 1, 2, 3 ]
```

1. Read '[', push a list onto the top of the stack.
2. Read '1' (whitespace outside of strings is ignored), Append integer 1 to list at top of stack.
3. Read '2' (whitespace and commas outside of strings are ignored), Append integer 2 to list at top of stack.
4. Read '3' (whitespace and commas outside of strings are ignored), Append integer 3 to list at top of stack.
5. Read ']', pop list off of the stack.
6. Since stack is empty, that is the result of the parse.

## Input 2

```
[ "A" ]
```

1. Read '[', push a list onto the top of the stack.
2. Read '"' (whitespace outside of strings is ignored), push an empty string onto the stack.
2. Read 'A', append A to the end of the string on the top of the stack
2. Read '"' pop a string off of the top of the stack.

