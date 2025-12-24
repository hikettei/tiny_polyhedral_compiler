import caten as C

class CPUTensor(C.ATenBase):
    def allocate(self):
        pass

    def free(self):
        pass

    def compile(self):
        pass

C.ATenBase.register("CPU", CPUTensor)
