from abc import ABC, abstractmethod
from typing import Dict, List, Union, Optional

class MirandaType(ABC):
    @abstractmethod
    def __str__(self) -> str:
        pass

class Num(MirandaType):
    def __str__(self) -> str:
        return "num"

class Bool(MirandaType):
    def __str__(self) -> str:
        return "bool"

class Char(MirandaType):
    def __str__(self) -> str:
        return "char"

class List(MirandaType):
    def __init__(self, elem_type: MirandaType):
        self.elem_type = elem_type

    def __str__(self) -> str:
        return f"[{self.elem_type}]"

class Tuple(MirandaType):
    def __init__(self, elem_types: List[MirandaType]):
        self.elem_types = elem_types

    def __str__(self) -> str:
        return f"({','.join(str(t) for t in self.elem_types)})"

class Function(MirandaType):
    def __init__(self, param_type: MirandaType, return_type: MirandaType):
        self.param_type = param_type
        self.return_type = return_type

    def __str__(self) -> str:
        return f"{self.param_type} -> {self.return_type}"

class TypeVar(MirandaType):
    def __init__(self, name: str):
        self.name = name

    def __str__(self) -> str:
        return self.name

class UserDefined(MirandaType):
    def __init__(self, name: str):
        self.name = name

    def __str__(self) -> str:
        return self.name

TypeEnv = Dict[str, MirandaType]

class Expr(ABC):
    @abstractmethod
    def infer_type(self, env: TypeEnv, counter: List[int]) -> MirandaType:
        pass

class Literal(Expr):
    def __init__(self, value: Union[int, float, bool, str]):
        self.value = value

    def infer_type(self, env: TypeEnv, counter: List[int]) -> MirandaType:
        if isinstance(self.value, (int, float)):
            return Num()
        elif isinstance(self.value, bool):
            return Bool()
        elif isinstance(self.value, str):
            if len(self.value) == 1:
                return Char()
            else:
                return List(Char())
        else:
            raise TypeError(f"Unknown literal type: {type(self.value)}")

class Var(Expr):
    def __init__(self, name: str):
        self.name = name

    def infer_type(self, env: TypeEnv, counter: List[int]) -> MirandaType:
        if self.name not in env:
            raise NameError(f"Unbound variable: {self.name}")
        return env[self.name]

class Lambda(Expr):
    def __init__(self, param: str, body: Expr):
        self.param = param
        self.body = body

    def infer_type(self, env: TypeEnv, counter: List[int]) -> MirandaType:
        param_type = TypeVar(f"*{counter[0]}")
        counter[0] += 1
        new_env = env.copy()
        new_env[self.param] = param_type
        body_type = self.body.infer_type(new_env, counter)
        return Function(param_type, body_type)

class Apply(Expr):
    def __init__(self, func: Expr, arg: Expr):
        self.func = func
        self.arg = arg

    def infer_type(self, env: TypeEnv, counter: List[int]) -> MirandaType:
        func_type = self.func.infer_type(env, counter)
        arg_type = self.arg.infer_type(env, counter)
        if isinstance(func_type, Function):
            if func_type.param_type == arg_type:
                return func_type.return_type
            else:
                raise TypeError("Function application type mismatch")
        else:
            raise TypeError("Cannot apply non-function type")

class Let(Expr):
    def __init__(self, name: str, value: Expr, body: Expr):
        self.name = name
        self.value = value
        self.body = body

    def infer_type(self, env: TypeEnv, counter: List[int]) -> MirandaType:
        value_type = self.value.infer_type(env, counter)
        new_env = env.copy()
        new_env[self.name] = value_type
        return self.body.infer_type(new_env, counter)

class ListExpr(Expr):
    def __init__(self, elements: List[Expr]):
        self.elements = elements

    def infer_type(self, env: TypeEnv, counter: List[int]) -> MirandaType:
        if not self.elements:
            raise ValueError("Cannot infer type of empty list")
        elem_type = self.elements[0].infer_type(env, counter)
        for elem in self.elements[1:]:
            if elem.infer_type(env, counter) != elem_type:
                raise TypeError("Inconsistent types in list")
        return List(elem_type)

class TupleExpr(Expr):
    def __init__(self, elements: List[Expr]):
        self.elements = elements

    def infer_type(self, env: TypeEnv, counter: List[int]) -> MirandaType:
        return Tuple([elem.infer_type(env, counter) for elem in self.elements])

def type_check(env: TypeEnv, expr: Expr, expected_type: MirandaType) -> None:
    counter = [0]
    inferred_type = expr.infer_type(env, counter)
    if inferred_type != expected_type:
        raise TypeError(f"Type mismatch: expected {expected_type}, got {inferred_type}")