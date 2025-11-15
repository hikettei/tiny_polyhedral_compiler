from caten.polyhedral.schedule_tree import ScheduleTree


class Band(ScheduleTree):
    def __init__(self) -> None:
        pass

    def decompose(self) -> None:
        # defactor Band(where size is A => A*B)
        pass
