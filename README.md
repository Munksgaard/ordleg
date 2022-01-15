# Ordleg

A wordle clone in Danish.

## Word list

Using
[RO2012.opslagsord.med.homnr.og.ordklasse.txt](https://dsn.dk/wp-content/uploads/2021/03/RO2012.opslagsord.med_.homnr_.og_.ordklasse.zip),
we can compute the list of possible words like this:

```
cut -f 1 -d';' RO2012.opslagsord.med.homnr.og.ordklasse.txt \
  | grep -E "^[a-zæøå]{5}$" \
  | tr [a-zæøå] [A-ZÆØÅ] \
  | sort \
  | sed 's/\(.*\)/"\1"/' \
  | paste -sd, - \
  >> ../src/ordle/src/Main.elm
```
