# hs-bench-build

Benchmark the compilation time of your Haskell project.

Useful for finding out exactly how much time each module takes to compile.

The tool measures the time elapsed between each line in the compilation log 
so it will also benchmark linking time, binary installation time, etc.

## Installation
* clone the repo
* stack install

## Usage

If you only have one package under "packages" in your stack.yaml then you can start hs-bench-build like so:

```
$ stack build 2>&1 | hs-bench-build
```

If you have more than one package then stack doesn't output detailed compilation info
to the console but writes it to a log file instead. In this case you will need to provide 
a path to the log file in addition to using the pipe:

```
$ stack build 2>&1 | hs-bench-build --file .stack-work/logs/myapp-0.5.1.log
```

Please note that you must pipe the output of stack to hs-bench-build even if you provide the log file.
This is necessary for the tool to detect when stack finishes compiling.

## Sample output
```
$ stack build --fast 2>&1 | hs-bench-build

Reading log lines from stdin...
-----------------------------
Biggest offenders at the top:
-----------------------------
5.30 sec: Compiling Model.CoreTypes
1.75 sec: Linking .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/myapp/myapp...
1.62 sec: Compiling Model.ResponseTypes
1.08 sec: Installing executable myapp in /home/dgaw/Projects/myapp/.stack-wo...
1.05 sec: Compiling Main
0.83 sec: Compiling Web.App
0.70 sec: Compiling Web.Actions.Website
0.66 sec: Compiling Web.Actions.User
...
```
