#include <iostream>
#include <map>
#ifdef _MSC_VER
#include <string>
#endif
#include <vector>

//Lexer
enum Token {
	tok_eof = -1,
	tok_def = -2,
	tok_extern = -3,
	tok_identifier = -4,
	tok_number = -5
};

static std::string IdentifierStr;
static double NumVal;

static int get_tok() {//return the next token from standard input
	static int LastChar = ' ';

	while (isspace(LastChar))
		LastChar = getchar();

	if (isalpha(LastChar)) {//[a-zA-Z][a-zA-Z0-9]*
		IdentifierStr = LastChar;
		while (isalnum(LastChar = getchar()))
			IdentifierStr += LastChar;

		if (IdentifierStr == "def")
			return tok_def;
		if (IdentifierStr =="extern")
				return tok_extern;
		return tok_identifier;
	}

	if (isdigit(LastChar) || LastChar == '.') {//[0-9.]+
		std::string NumStr;
		do {
			NumStr += LastChar;
			LastChar = getchar();
		} while (isdigit(LastChar) || LastChar == '.');

		NumVal = std::stod(NumStr, 0);
		return tok_number;
	}

	if (LastChar == '#') {
		std::string comment;
		std::getline(std::cin, comment);
	}

	if (LastChar == EOF)
		return tok_eof;

	int ThisChar = LastChar;
	LastChar = getchar();
	return ThisChar;
}

//Abstract Syntax Tree
namespace {
	class ExprAST {//base class for all expression nodes
	public:
		virtual ~ExprAST() {}
	};

	class NumberExprAST : public ExprAST {//numeric literals
		double Val;
	public:
		explicit NumberExprAST(double val) : Val(val) {}
	};

	class VariableExprAST : public ExprAST {//variable references
		std::string Name;
	public:
		explicit VariableExprAST(const std::string& name) : Name(name) {}
	};

	class BinaryExprAST : public ExprAST {//binary operators
		char Op;
		ExprAST *LHS, *RHS;
	public:
		BinaryExprAST(char op, ExprAST *lhs, ExprAST *rhs) : Op(op), LHS(lhs), RHS(rhs) {}
	};

	class CallExprAST : public ExprAST {
		std::string Callee;
		std::vector<ExprAST*> Args;
	public:
		CallExprAST(const std::string& callee, std::vector<ExprAST*>& args) : Callee(callee), Args(args) {}
	};

	class PrototypeAST {//function interface
		std::string Name;
		std::vector<std::string> Args;
	public:
		PrototypeAST(const std::string& name, const std::vector<std::string>& args) : Name(name), Args(args) {}
	};

	class FunctionAST {//function definition/implementation
		PrototypeAST *Proto;
		ExprAST *Body;
	public:
		FunctionAST(PrototypeAST *proto, ExprAST *body) : Proto(proto), Body(body) {}
	};
}

static int CurTok;
static int getNextToken() {
	return CurTok = get_tok();
}

static std::map<char, int> BinopPrecedence;//

static int GetTokPrecedence() {//get the precedence of the pending binary operator token
	if (!isascii(CurTok))
		return -1;
	int TokPrec = BinopPrecedence[CurTok];
	if (TokPrec <= 0)
		return -1;
	return TokPrec;
}

void Error(const std::string& Str) {
	std::cerr << "Error: " << Str << std::endl;
}

static ExprAST *ParseExpression();

/*identifierexpr
		::= identifier
		::= identifier '(' expression *')'
 */
static ExprAST *ParseIdentifierExpr() {
	std::string IdName = IdentifierStr;
	getNextToken();
	if (CurTok != '(')
		return new VariableExprAST(IdName);

	//Call
	getNextToken();//eat '('
	std::vector<ExprAST*> Args;
	while (CurTok != ')') {
		ExprAST *Arg = ParseExpression();
		if (!Arg)
			return nullptr;
		Args.push_back(Arg);
		if (CurTok == ',')
			getNextToken();
		else if (CurTok != ')') {
			Error("Expect ')' or ',' in argument list");
			return nullptr;
		}
	}
	getNextToken();//eat ')'
	return new CallExprAST(IdName, Args);
}

/*numberexpr
		::= number
 */
static ExprAST *ParseNumberExpr() {
	ExprAST *Result = new NumberExprAST(NumVal);
	getNextToken();
	return Result;
}

/*parenexpr
		::=
 */
static ExprAST *ParseParenExpr() {
	getNextToken();//eat '('
	ExprAST *V = ParseExpression();
	if (!V)
		return nullptr;
	if (CurTok != ')') {
		Error("expected ')'");
		return nullptr;
	}
	getNextToken();//eat ')'
	return V;
}

/*primary
		::= identifierexpr
		::= numberexpr
		::= parenexpr
 */
static ExprAST *ParsePrimary() {
	switch (CurTok) {
		case tok_identifier:
			return ParseIdentifierExpr();
		case tok_number:
			return ParseNumberExpr();
		case '(':
			return ParseParenExpr();
		default:
			Error("unknown token when expecting an expression");
			return nullptr;
	}
}

/*binoprhs
		::= ('+' primary)*
 */
static ExprAST *ParseBinOpRHS(int ExprPrec, ExprAST *LHS) {
	while (true) {
		int TokPrec = GetTokPrecedence();
		if (TokPrec < ExprPrec)
			return LHS;
		
		char BinOp = CurTok;
		getNextToken();

		ExprAST *RHS = ParsePrimary();
		if (!RHS)
			return nullptr;

		int NextPrec = GetTokPrecedence();
		if (TokPrec < NextPrec) {
			RHS = ParseBinOpRHS(TokPrec + 1, RHS);
			if (!RHS)
				return nullptr;
		}

		LHS = new BinaryExprAST(BinOp, LHS, RHS);
	}
}

/*expression
		::= primary binoprhs
 */
static ExprAST *ParseExpression() {
	ExprAST *LHS = ParsePrimary();
	if (!LHS)
		return nullptr;
	return ParseBinOpRHS(0, LHS);
}

/*prototype
		::= id '(' id *')'
 */
static PrototypeAST *ParsePrototype() {
	if (CurTok != tok_identifier) {
		Error("expected function name in protytype");
		return nullptr;
	}

	std::string FnName = IdentifierStr;
	getNextToken();
	if (CurTok != '(') {
		Error("expected '(' in prototype");
		return nullptr;
	}

	std::vector<std::string> ArgNames;
	while (getNextToken() == tok_identifier)
		ArgNames.push_back(IdentifierStr);
	if (CurTok != ')') {
		Error("expected ')' in prototype");
		return nullptr;
	}

	getNextToken();//eat ')'
	return new PrototypeAST(FnName, ArgNames);
}

/*definition
		::= "def" prototype expression
 */
static FunctionAST *ParseDefinition() {
	getNextToken();//eat "def"
	PrototypeAST *Proto = ParsePrototype();
	if (Proto == nullptr)
		return nullptr;
	if (ExprAST *E = ParseExpression())
		return new FunctionAST(Proto, E);
	return nullptr;
}

/*toplevelexpr
		::= expression
 */
static FunctionAST *ParseTopLevelExpr() {
	if (ExprAST *E = ParseExpression()) {
		PrototypeAST *Proto = new PrototypeAST("", std::vector<std::string>());
		return new FunctionAST(Proto, E);
	}
	return nullptr;
}

/*external
		::= "extern" prototype
 */
static PrototypeAST *ParseExtern() {
	getNextToken();//eat "extern"
	return ParsePrototype();
}

//Top-level parsing
static void HandleDefinition() {
	if (ParseDefinition())
		std::cerr << "function definition parsed" << std::endl;
	else
		getNextToken();
}

static void HandleExtern() {
	if (ParseExtern())
		std::cerr << "extern parsed" << std::endl;
	else
		getNextToken();
}

static void HandleTopLevelExpression() {
	if (ParseTopLevelExpr())
		std::cerr << "top-level expr parsed" << std::endl;
	else
		getNextToken();
}

/*top
		:: = definition | external | expression | ';'
 */
static void MainLoop() {
	while (true) {
		std::cerr << "ready> ";
		getNextToken();
		switch (CurTok) {
			case tok_eof:
				return;
			case ';':
				//getNextToken();
				break;
			case tok_def:
				HandleDefinition();
				break;
			case tok_extern:
				HandleExtern();
				break;
			default:
				HandleTopLevelExpression();
		}
	}
}

int main() {
	//Install standard binary operators
	BinopPrecedence['<'] = 10;
	BinopPrecedence['+'] = 20;
	BinopPrecedence['-'] = 20;
	BinopPrecedence['*'] = 40;

	//Prime the first token
	//std::cerr << "ready> ";
	//getNextToken();

	MainLoop();
	return 0;
}
