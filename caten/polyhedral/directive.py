from abc import ABCMeta, abstractmethod


class Directive(metaclass=ABCMeta):
    """
    Directive is a hackable object which transforms
    """
    @abstractmethod
    def on_schedule(self) -> None:
        pass
    
    @abstractmethod
    def on_ast(self) -> None:
        pass

class Parallel(Directive):
    def on_schedule(self) -> None:
        pass

    def on_ast(self) -> None:
        pass
