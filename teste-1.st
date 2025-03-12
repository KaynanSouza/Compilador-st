PROGRAM ExampleProgram
    (* ola mundo *)
    VAR
    i : INT;
	iCounter : INT := 0;
	bCondition : BOOL := FALSE;
	fResult : REAL := 0.0;
	xBitwise : BYTE := 16#ff;
	arrData : ARRAY [1..10] OF INT;
	strMessage : STRING[50] := '';
	dtCurrent : DATE_AND_TIME;
	dwWord : DWORD := 16#FFFF_FFFF;
	uINTVar : UINT := 65000;
	usTime : TIME := T#1s;
    END_VAR

    VAR CONSTANT
	pi : REAL := 3.14159;
	cMaxValue : INT := 100;
    END_VAR

    VAR RETAIN
	retainedValue : INT := 10;
    END_VAR

    IF iCounter = 0 THEN
	strMessage := 'Programa iniciado!';
	iCounter := 1;
    END_IF

    (* Exemplo de laço FOR *)
    FOR i := 1 TO 10 DO
	arrData[i] := i * 2;
    END_FOR

    (* Exemplo de laço WHILE *)
    WHILE iCounter <= cMaxValue DO
	iCounter := iCounter + 1;
    END_WHILE


    CASE iCounter OF
	0: strMessage := 'Zerado!';
	1..50: strMessage := 'Entre 1 e 50';
	51..100: strMessage := 'Entre 51 e 100';
    ELSE
	strMessage := 'Fora do intervalo!';
    END_CASE

    xBitwise := xBitwise AND 16#F0;
    xBitwise := xBitwise OR 16#0F;
    xBitwise := NOT xBitwise;


    IF bCondition THEN
	fResult := fResult + 1.0;
    ELSIF NOT bCondition THEN
	fResult := fResult - 1.0;
    ELSE
	fResult := 0.0;
    END_IF

    REPEAT
	iCounter := iCounter - 1;
    UNTIL iCounter = 0
    END_REPEAT

    bCondition := (iCounter = 0) AND (fResult > 0.0);

    dtCurrent := DT#2024-11-28-00:00:00;

END_PROGRAM