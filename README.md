# hs-bench-build

Benchmark the compilation time of your Haskell modules using the stack log file.

Useful for finding which of your modules use too many fancy language features. :)

## Installation
* clone the repo
* stack install

## Usage
```
$ hs-bench-build .stack-work/logs/myapp-0.5.1.log
```
Once started the tool will monitor your log file for changes 
so start your `stack build` in a separate terminal and wait for it to finish.

## Sample output
```
$ hs-bench-build ./myapp/.stack-work/logs/myapp-0.5.1.log

Checking the log file every 100 ms. Run your stack build now.
Biggest offenders at the top:
-----------------------------
Model.CoreTypes: 5.215 sec 
Model.ResponseTypes: 1.604 sec 
Web.App: 0.902 sec 
Main: 0.902 sec 
Web.Actions.Website: 0.702 sec 
Web.Actions.User: 0.702 sec 
...
```
