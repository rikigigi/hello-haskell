
# Hello haskell first program

computes the sum of keyword in the form `keyword amount`, where `amount` is a double.
The list is coded inside the program

# compilation

```ghc Hello.hs -o Hello```

# usage 
```
./Hello
tarallini 10 pane 120 yogurt 100
```
output:
```
(["tarallini","190.0","pane","324.0","yogurt","100.0"],"",614.0)
```
It displays errors too:
```
./Hello
pizza 50
```
output:
```
(["pizza","50.0"],"Error: undefined food 'pizza' before col 6\n",50.0)
```
