
class Constrainted():
    pass
# ConstraintedScheduleModel
# Parametric Tiling
# finalize() -> Schedule
# s.permute("ijk -> ikj") | s["ijk -> ikj"]
# domain.reshape is needed
# ScheduleNodeBand TODO List
# - Force Isolation Option (Loop Reminder) GPU Guard or Reminder Generation
# - Insert Mark
# - Paddingができるようにする
# NotebookにTransformationの一覧を記述する
# Symbolic Tileどこ？
# Building Mode vs After Build Mode
# - 両方で，統一的に，ループ変換を行える抽象化は何？
# Construction Phase => Optimization Phase
# with (P.directive("gpu") | P.domain(...)) as f:
#   f = (f @ [3, 3])
#   同じStyleを2回繰り返して書く
# with P.domain(...) as f: # BEAM Searchするから，こっちもTree, directive.py
#   pass
# Polyhedral Scalar Representation
# Directive, Baseの二つのデータ構造はある
# - or, transformationをlazy evalする
# https://dl.acm.org/doi/epdf/10.1145/3178372.3179509
# https://inria.hal.science/hal-02493164v2/document
# 後もう一本読むべき論文があったはず・・・
# Symbolic Tileを実現するには，Domain <-> Bandの間で，計算グラフを構築する必要がある。
