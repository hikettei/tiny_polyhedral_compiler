from pydantic import BaseModel
from abc import ABCMeta, abstractmethod

import caten.isl as I

class Directive(metaclass=ABCMeta):
    """
    Directive is a hackable object which transforms
    """
    @abstractmethod
    def on_schedule(self):
        pass
    
    @abstractmethod
    def on_ast(self):
        pass
