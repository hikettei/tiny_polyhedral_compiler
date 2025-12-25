import caten as C

class CPUTensor(C.ATenBase):
    def allocate(self):
        pass

    def free(self):
        pass

    #@staticmethod
    def compile(self):
        pass

    @staticmethod
    def render(op):
        def _render(node):
            pass

C.ATenBase.register("CPU", CPUTensor)
