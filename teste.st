PROGRAM MyProg
VAR
  x : INT := 10;
  y : REAL := 20.5;
  texto : STRING := "hello";
  cond : BOOL := TRUE;
  vetor : ARRAY[1..5] OF INT := [1, 2, 3, 4, 5];
  vetor2 : ARRAY[1..5] OF INT := [2(1), 3, 4, 5, 5];
END_VAR

x := x + 2;

IF x < 20 THEN
  x := x + 1;
  vetor[2] := x * 2;
ELSE
  x := x - 5;
  vetor[3] := x + 10;
END_IF

IF cond THEN
  texto := "condicao verdadeira";
ELSE
  texto := "condicao falsa";
END_IF

vetor[1] := vetor[1] + vetor[2];

END_PROGRAM
