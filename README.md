# SimpleJsonParser
A basic JSON parser written in F# using FParsec

The parser can evaluate a JSON expression of any depth. It identifies the string and floating point values along with key value pairs.

In addition to this, the application can pretty-print the parsed JSON, and has additional (very basic) functionality to simplify a math equation and solve it.

Sample usage:
```
> dotnet run "sample.json"
Parsed: "((1 + (x * 10)) = 21)"
Simplified: x = "((21 - 1) / 10)"
Solution: "(x = 2)"
```
