# CFG-TO-CNF
##### Conversion of Context-Free Grammar to Chomsky Normal Form
---
The program reads a grammar from a file or stdin, checks the validity of the grammar and converts it to Chomsky Normal Form (CNF). The program is compiled using a makefile. It can be run as ./cfg-to-cnf option [file], where options can be:

- `-i` - Parse the grammar from an input and check if it is valid. This option checks if the format of the input is the same as specified in format input. It also checks if all of the non-terminals and terminals that appear in the rules are also in the groups of non-terminals and terminals. The grammar is then printed to stdout. 

- `-1` - Convert the grammar G = (N,T,P,S) to grammar G'=(N,T,P',S) withouth simple rules (N->N), while keeping the language of the grammars the same L(G) = L(G'). The new grammar is then printed to stdout.

- `-2` - Convert the grammar G = (N,T,P,S) to grammar G'=(N',T,P',S) in CNF. The input grammar G is first converted to grammar without simple rules and subsequently to CNF. The grammar in CNF only has rules in the form of N->NN and N->T.

The input takes a group of nonterminals N from [A-Z], a group of terminals T from [a-z] and a group of rules in the form of N->(N U T)*. Format of the input:

```
<list of non-terminal symbols>\n     
<list of terminals symbols>\n
<initial non-terminal symbol>\n
<rule 1>\n
...
<rule N>\n
```

##### Example of an input and expected outcomes:

---

```
Input:			
E,T,F
a,b,c,d
E
E->EaT
E->T
T->TbF
T->F
F->cEc
F->d
```

```
./cfg-to-cnf -i < input
E,T,F
a,b,c,d
E
E->EaT
E->T
T->TbF
T->F
F->cEc
F->d
```

```
./cfg-to-cnf -1 < input
E,T,F
a,b,c,d
E
E->EaT
E->TbF
E->cEc
E->d
T->TbF
T->cEc
T->d
F->cEc
F->d
```

```
./cfg-to-cnf -2 < input
E,T,F,<aT>,a',<bF>,b',c',<Ec>
a,b,c,d
E
E->d
T->d
F->d
E->E<aT>
<aT>->a'T
a'->a
E->T<bF>
<bF>->b'F
b'->b
E->c'<Ec>
c'->c
<Ec>->Ec'
T->T<bF>
T->c'<Ec>
F->c'<Ec>
```