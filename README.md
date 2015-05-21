# hmailfilter

Managing rules with procmail is too cumbersome for me (specially if genericty is required)
So instead this programme with parse email headers, use the associated rules, and print the destination folder for procmail.

You can see rule example in src/Main.hs
```haskell
let rules = [  --If all Filters Match  then move email to this Dir  
            ,  [pourMoi]      ->> const "./"
            ,  [devNull]      ->> const "/dev/null"
            ,  [tabulaRasa]   ->> const ".Compte.TabulaRasa/"
            ,  [pourDomaine]  ->> \hs -> ".Compte." <> virtualUser hs <> "/"
            ]         
```

and procmailrc example in procmailrc file
```bash
DEST=`formail -X "" | hmailfilter`
:0:
* .
$DEST
```

## How to run ? 
##### To install (only once):
1. cabal sandbox init
2. Edit your rules
3. cabal install
4. sudo cp .cabal-sandbox/bin/hmailfilter /usr/local/bin

##### Now to run: 
1. Edit your procmailrc and use procmail to run the mail filter



