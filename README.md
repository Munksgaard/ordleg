# Ordleg

A wordle clone in Danish.

## Word list

Using
[RO2012.opslagsord.med.homnr.og.ordklasse.txt](https://dsn.dk/wp-content/uploads/2021/03/RO2012.opslagsord.med_.homnr_.og_.ordklasse.zip),
we can compute the list of possible words like this:

```
cut -f 1 -d';' RO2012.opslagsord.med.homnr.og.ordklasse.txt \
  | grep -E "^[0-9\. ]*[a-zæøå]{5}$" \
  | sed 's/[0-9\. ]*\(.*\)/"\1"/' \
  | sort \
  | uniq \
  | tr [a-zæøå] [A-ZÆØÅ] \
  | paste -sd, - \
  >> src/Main.elm
```
