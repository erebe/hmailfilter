# hmailfilter

Managing rules with procmail is too cumbersome for me (especially if genericity is required)
So instead this programme will parse email headers, use the associated rules, and print the destination folder for procmail to use it.

You can see rule and filter examples in src/Main.

Filter examples
```haskell
tabulaRasa :: Match Any
tabulaRasa = for $ anyOf ["tabula.rasa@erebe.eu", "editeur.algo@erebe.eu"]

haskellCafe :: Match Any
haskellCafe = mailingList $ anyOf ["haskell-cafe"]

-- Match if ANY of those 3 matches is a hit !
blacklist :: Match Any
blacklist =    from (anyOf [".Meds="])
            <> for (anyOf ["mediapart@"])
            <> subject (anyOf ["pussy", "naked", "fuck"])

-- Match if ALL of those 2 matches is a hit !
fromGod :: Match All
fromGod =      from (anyOf ["God"])
            <> subject (anyOf ["It's God talking"])


```
Rule examples
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
DEST=`hmailfilter`
:0:
* .
$DEST
```
## What to expect
1. Can parse a 62M header only email, without an flinch
2. Parse header value that is encoded (Ex: =?UTF-8?B?ABB?= or =?iso-8859-1?Q?=A1Hola,_se=F1or!?=)

## How to run ? 
##### To install (only once):
1. stack init
2. stack install

##### Now to run: 
1. Edit your procmailrc and use procmail to run the mail filter

## Todo 
1. Add some reader Monad instead of Hashmap -> Parser HashMap


