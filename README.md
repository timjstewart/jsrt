# jsrt - JavaScript Round Trip

jsrt is a tool that can extract JavaScript function bodies out of a JSON file
and write them all to a JavaScript file. The JavaScript functions are annotated
with a comment in such a way that if the function bodies are modified, they can
be merged back into the original JavaScript file.

## Configuration

The configuration for jsrt should be found in `~/.config/jsrt/jsrt.conf`. An
example configuration follows:

```
# The pattern that will locate the name of the function
functionNamePattern = **.functionName

# The pattern that will locate the function body
exportPattern = **.code
```

## Example JSON in file named test.json

```
[
    {
        "top": {
            "blocks": [
                {
                    "functionName": "alert block one",
                    "code": "console.alert('Hi block one');"
                },
                {
                    "functionName": "alert block two",
                    "code": "console.alert('Hi block two');"
                }
            ]
        }
    },
    {
        "top": {
            "blocks": [
                {
                    "functionName": "enumerate over strings",
                    "code": "
                    // block three
                    var x = new Date();
                    _.forEach(['one', 'two', 'three'], function(number) {
                        console.alert(number);
                    });
                    console.alert('Hi three', x);"
                },
                {
                    "functionName": "alert block four",
                    "code": "console.alert('Hi block four');"
                }
            ]
        }
    }
]

```

## Running the command

```
jsrt test.json
```

## Resulting JavaScript in test.json.js

```
/** path([0].top.blocks[0].code) */
function alert_block_one() {
    console.alert('Hi block one');
}

/** path([0].top.blocks[1].code) */
function alert_block_two() {
    console.alert('Hi block two');
}

/** path([1].top.blocks[0].code) */
function enumerate_over_strings() {
    // block three
    var x = new Date();
    _.forEach(['one', 'two', 'three'], function(number) {
        console.alert(number);
    });
    console.alert('Hi three', x);
}

/** path([1].top.blocks[1].code) */
function alert_block_four() {
    console.alert('Hi block four');
}
```
