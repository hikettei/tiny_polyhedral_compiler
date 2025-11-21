from abc import ABC, abstractmethod
from typing import Any, List

from .ops import Node


class CompiledKernel(ABC):
    @abstractmethod
    def __call__(self, *args: Any, **kwargs: Any) -> Any: pass

class Runtime(ABC):
    @abstractmethod
    def compile(self, graph_nodes: List[Node], input_placeholders: List[Node]) -> CompiledKernel: pass
