#include <iostream>
#include <map>
#ifdef _MSC_VER
#include <string>
#endif
#include <vector>

#include <llvm/ADT/APFloat.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

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
		virtual llvm::Value* Codegen() {
			return nullptr;
		}
	};

	class NumberExprAST : public ExprAST {//numeric literals
		double Val;
	public:
		explicit NumberExprAST(double val) : Val(val) {}
		virtual llvm::Value* Codegen();
	};

	class VariableExprAST : public ExprAST {//variable references
		std::string Name;
	public:
		explicit VariableExprAST(const std::string& name) : Name(name) {}
		llvm::Value* Codegen();
	};

	class BinaryExprAST : public ExprAST {//binary operators
		char Op;
		ExprAST* LHS;
		ExprAST* RHS;
	public:
		BinaryExprAST(char op, ExprAST* lhs, ExprAST* rhs) : Op(op), LHS(lhs), RHS(rhs) {}
		llvm::Value* Codegen();
	};

	class CallExprAST : public ExprAST {
		std::string Callee;
		std::vector<ExprAST*> Args;
	public:
		CallExprAST(const std::string& callee, std::vector<ExprAST*>& args) : Callee(callee), Args(args) {}
		llvm::Value* Codegen();
	};

	class PrototypeAST {//function interface
		std::string Name;
		std::vector<std::string> Args;
	public:
		PrototypeAST(const std::string& name, const std::vector<std::string>& args) : Name(name), Args(args) {}
		llvm::Function* Codegen();
	};

	class FunctionAST {//function definition/implementation
		PrototypeAST* Proto;
		ExprAST* Body;
	public:
		FunctionAST(PrototypeAST* proto, ExprAST* body) : Proto(proto), Body(body) {}
		llvm::Function* Codegen();
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

static ExprAST* ParseExpression();

/*identifierexpr
		::= identifier
		::= identifier '(' expression* ')'
 */
static ExprAST* ParseIdentifierExpr() {
	std::string IdName = IdentifierStr;
	getNextToken();
	if (CurTok != '(')
		return new VariableExprAST(IdName);

	//Call
	getNextToken();//eat '('
	std::vector<ExprAST*> Args;
	while (CurTok != ')') {
		ExprAST* Arg = ParseExpression();
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
static ExprAST* ParseNumberExpr() {
	ExprAST* Result = new NumberExprAST(NumVal);
	getNextToken();
	return Result;
}

/*parenexpr
		::=
 */
static ExprAST* ParseParenExpr() {
	getNextToken();//eat '('
	ExprAST* V = ParseExpression();
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
static ExprAST* ParsePrimary() {
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
static ExprAST* ParseBinOpRHS(int ExprPrec, ExprAST* LHS) {
	while (true) {
		int TokPrec = GetTokPrecedence();
		if (TokPrec < ExprPrec)
			return LHS;
		
		char BinOp = CurTok;
		getNextToken();

		ExprAST* RHS = ParsePrimary();
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
static ExprAST* ParseExpression() {
	ExprAST* LHS = ParsePrimary();
	if (!LHS)
		return nullptr;
	return ParseBinOpRHS(0, LHS);
}

/*prototype
		::= id '(' id* ')'
 */
static PrototypeAST* ParsePrototype() {
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
static FunctionAST* ParseDefinition() {
	getNextToken();//eat "def"
	PrototypeAST* Proto = ParsePrototype();
	if (Proto == nullptr)
		return nullptr;
	if (ExprAST* E = ParseExpression())
		return new FunctionAST(Proto, E);
	return nullptr;
}

/*toplevelexpr
		::= expression
 */
static FunctionAST* ParseTopLevelExpr() {
	if (ExprAST* E = ParseExpression()) {
		PrototypeAST* Proto = new PrototypeAST("", std::vector<std::string>());
		return new FunctionAST(Proto, E);
	}
	return nullptr;
}

/*external
		::= "extern" prototype
 */
static PrototypeAST* ParseExtern() {
	getNextToken();//eat "extern"
	return ParsePrototype();
}

//Code Generation
static llvm::Module* TheModule;
static llvm::IRBuilder<> Builder(llvm::getGlobalContext());
static std::map<std::string, llvm::Value*> NamedValues;

llvm::Value* NumberExprAST::Codegen() {
	return llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(Val));
}

llvm::Value* VariableExprAST::Codegen() {
	llvm::Value* V = NamedValues[Name];
	if (!V) {
		Error("unknown variable name");
		return nullptr;
	}
	return V;
}

llvm::Value* BinaryExprAST::Codegen() {
	llvm::Value* L = LHS->Codegen();
	llvm::Value* R = RHS->Codegen();
	if (!L || !R)
		return nullptr;
	switch (Op) {
		case '+':
			return Builder.CreateFAdd(L, R, "addtmp");
		case '-':
			return Builder.CreateFSub(L, R, "subtmp");
		case '*':
			return Builder.CreateFMul(L, R, "multmp");
		case '<':
			L = Builder.CreateFCmpULT(L, R, "cmptmp");
			return Builder.CreateUIToFP(L, llvm::Type::getDoubleTy(llvm::getGlobalContext()), "booltmp");
		default:
			Error("invalid binary operator");
			return nullptr;
	}
}

llvm::Value* CallExprAST::Codegen() {
	llvm::Function* CalleeF = TheModule->getFunction(Callee);
	if (!CalleeF) {
		Error("unknown function referenced");
		return nullptr;
	}
	if (CalleeF->arg_size() != Args.size()) {
		Error("Incorrect # arguments passed");
		return nullptr;
	}

	std::vector<llvm::Value*> ArgsV;
	for (auto& arg : Args) {
		ArgsV.push_back(arg->Codegen());
		if (ArgsV.back() == nullptr)
			return nullptr;
	}
	return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

llvm::Function* PrototypeAST::Codegen() {
	std::vector<llvm::Type*> Doubles(Args.size(), llvm::Type::getDoubleTy(llvm::getGlobalContext()));
	llvm::FunctionType* FT = llvm::FunctionType::get(llvm::Type::getDoubleTy(llvm::getGlobalContext()), Doubles, false);
	llvm::Function* F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, Name, TheModule);

	if (F->getName() != Name) {
		F->eraseFromParent();
		F = TheModule->getFunction(Name);
		if (!F->empty()) {
			Error("redefinition of function");
			return nullptr;
		}
		if (F->arg_size() != Args.size()) {
			Error("redefinition of function with different # args");
			return nullptr;
		}
		unsigned Idx = 0;
		for (llvm::Function::arg_iterator AI = F->arg_begin(); Idx != Args.size(); ++AI) {
			AI->setName(Args[Idx]);

			// Add arguments to variable symbol table.
			NamedValues[Args[Idx]] = AI;
			++Idx;
		}
	}
	return F;
}

llvm::Function* FunctionAST::Codegen() {
	NamedValues.clear();
	llvm::Function* TheFunction = Proto->Codegen();
	return TheFunction;
}

//Top-Level Parsing
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
	llvm::LLVMContext& Context = llvm::getGlobalContext();

	//Install standard binary operators
	BinopPrecedence['<'] = 10;
	BinopPrecedence['+'] = 20;
	BinopPrecedence['-'] = 20;
	BinopPrecedence['*'] = 40;

	//Prime the first token
	//std::cerr << "ready> ";
	//getNextToken();
	
	TheModule = new llvm::Module("my cool jit", Context);

	MainLoop();

	TheModule->dump();

	return 0;
}
