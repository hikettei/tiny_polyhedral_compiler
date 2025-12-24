from abc import ABCMeta, abstractmethod


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

class Parallel(Directive):
    def on_schedule(self):
        pass

    def on_ast(self):
        pass
