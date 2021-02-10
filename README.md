# HsTim

A qq interface written in Haskell. Based on Mirai.

## How to Build

```shell
stack build
```

or

```shell
stack install
```

## How to Use

  1. Put a `confg.json` in the same folder the executable is in. It should be like:
  ```json
  {
    "baseUrl": "127.0.0.1",
    "miraiPort": 1234,
    "authKey": "authKey",
    "qq": 1234567890
  }
  ```

  2. Run the executable.

  3. If an error occurs saying a `.so` file cannot be found, just locate which package it is in and install the package. Meanwhile, you can report it in issues so I can update this README.md and specify the needed package here.

  4. If you should encounter any bug while using HsTim, please report it in issues.