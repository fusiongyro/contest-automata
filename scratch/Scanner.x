{
module Scanner (Token(..), scan) where
}

%wrapper "basic"

tokens :-
	"//".*\n			;
	$white 				;
	"\n"				{ \_ -> TNewline }
	"->"				{ \_ -> TArrow   }
	"#"					{ \_ -> TPound   }
	":"					{ \_ -> TColon   }
	","					{ \_ -> TComma   }
	[a-z][a-zA-Z0-9]*	{ \s -> TState s }

{

data Token = TNewline
		   | TArrow
		   | TPound
		   | TColon
		   | TComma
		   | TState String
	deriving (Show, Eq)

scan = alexScanTokens
	
}