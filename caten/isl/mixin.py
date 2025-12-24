from typing import Any
from .obj import InPlace

class ISLObjectMixin:
    """
    Mixin class for ISL objects to provide operator overloading and high-level conveniences.
    Methods are delegated to the underlying ISL methods (e.g., union, intersect) if they exist.
    """
    
    def __or__(self, other: Any) -> Any:
        """Union (A | B)"""
        if hasattr(self, "union"):
            return self.union(other)
        return NotImplemented

    def __and__(self, other: Any) -> Any:
        """Intersection (A & B)"""
        if hasattr(self, "intersect"):
            return self.intersect(other)
        return NotImplemented

    def __sub__(self, other: Any) -> Any:
        """Set difference or arithmetic subtraction (A - B)"""
        if hasattr(self, "subtract"):
            return self.subtract(other)
        if hasattr(self, "sub"):
            return self.sub(other)
        return NotImplemented

    def __add__(self, other: Any) -> Any:
        """Arithmetic addition or sum (A + B)"""
        if hasattr(self, "add"):
            return self.add(other)
        if hasattr(self, "sum"):
            return self.sum(other)
        return NotImplemented

    def __mul__(self, other: Any) -> Any:
        """Multiplication (A * B)"""
        if hasattr(self, "mul"):
            return self.mul(other)
        if hasattr(self, "product"):
            return self.product(other)
        return NotImplemented

    def __le__(self, other: Any) -> bool:
        """Subset check (A <= B) or less-equal"""
        if hasattr(self, "is_subset"):
            return self.is_subset(other)
        if hasattr(self, "le"):
            return self.le(other)
        return NotImplemented

    def __lt__(self, other: Any) -> bool:
        """Strict subset check (A < B) or less-than"""
        if hasattr(self, "is_strict_subset"):
            return self.is_strict_subset(other)
        if hasattr(self, "lt"):
            return self.lt(other)
        return NotImplemented

    def __ge__(self, other: Any) -> bool:
        """Superset check (A >= B) or greater-equal"""
        # Superset is reverse subset
        if hasattr(self, "is_subset"):
            # If self >= other, then other <= self
            if hasattr(other, "is_subset"):
                return other.is_subset(self)
        if hasattr(self, "ge"):
            return self.ge(other)
        return NotImplemented

    def __gt__(self, other: Any) -> bool:
        """Strict superset check (A > B) or greater-than"""
        if hasattr(self, "is_strict_subset"):
            if hasattr(other, "is_strict_subset"):
                return other.is_strict_subset(self)
        if hasattr(self, "gt"):
            return self.gt(other)
        return NotImplemented

    def __eq__(self, other: Any) -> bool:
        """Equality check (A == B)"""
        if hasattr(self, "is_equal"):
            return self.is_equal(other)
        if hasattr(self, "eq"):
            return self.eq(other)
        return False

    def __ne__(self, other: Any) -> bool:
        """Inequality check (A != B)"""
        return not self.__eq__(other)
    
    def __neg__(self) -> Any:
        """Negation (-A)"""
        if hasattr(self, "neg"):
            return self.neg()
        return NotImplemented

    def __getitem__(self, key: Any) -> Any:
        """Array access (A[i]) or List access (L[i])"""
        if hasattr(self, "access"):
            # Handle multiple indices for access (e.g. A[i, j])
            if isinstance(key, tuple):
                return self.access(*key)
            return self.access(key)
        if hasattr(self, "get_at"):
             return self.get_at(key)
        raise TypeError(f"Object of type {type(self).__name__} does not support indexing (access or get_at).")

    def __setitem__(self, key: Any, value: Any) -> Any:
        if hasattr(self, "set_val"):
            return InPlace(self).set_val(key, value)
