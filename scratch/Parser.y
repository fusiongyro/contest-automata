{
module Main where
import Scanner
}

%name dfa
%tokentype { Token }
%error { parseError }
%token
    "nl"    { TNewline  }
    "->"    { TArrow    }
    "#"     { TPound    }
    ":"     { TColon    }
    ","     { TComma    }
    state   { TState $$ }

%%

DFA         : 
    StateList "#" Alphabet "#" Transitions "#" StartState "#" AcceptingStates { DFA $1 $3 $5 $7 $9 }

StateList   :
    state                   { StateList Empty $1 }
    | StateList "," state   { StateList $1 $3    }

Alphabet    :
    state                   { SymbolList Empty $1 }
    Alphabet "," state      { SymbolList $1 $3    }

