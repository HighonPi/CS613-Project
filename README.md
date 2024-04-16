# A Tool for Visualizing Reductions in Haskell

## Quick Start
In the project root directory first build the project using 
```shell
stack build
```
<p>

To start the tool run the following command
```shell
stack exec vTool
```
This starts a basic interactive console where you can enter commands of your choice

Run `--help` to see available options
```plain
Enter command: --help
A Tool for Visualizing Reductions in Haskell

Usage: vTool (step | list)

Available options:
  -h,--help                Show this help text

Available commands:
  step                     
  list
```

All available commands can be run with `--help` to get more info on how to use them.

### Stepping
Step through the Haskell Core version of an user program
```shell
Enter command: step --help
Usage: vTool step (-p|--path STRING) [-f|--function STRING] [-v|--verbose]

Available options:
  -h,--help                Show this help text
  -p,--path STRING         The Haskell source file used as input to the tool
  -f,--function STRING     Top level function to step through
  -v,--verbose             Enable verbose output if set
```
Note that you have to copy and place [This mini-custom Prelude file](./src/MiniPrelude.hs) in the same directory as your test program. 

Follow the template below for your test programs
```haskell
{-# OPTIONS -XNoImplicitPrelude #-}

module THE_FILE_NAME where

import MiniPrelude -- Not required

-- Your code below
```

### Listing
List all top level bindings in the Haskell Core version of an user program
```shell
Enter command: list --help
Usage: vTool list (-p|--path STRING)

Available options:
  -h,--help                Show this help text
  -p,--path STRING         The Haskell source file used as input to the tool
```