#!/usr/bin/python

import aoc_util
import re


class Component:
    def __init__(self):
        self.name = ""

    def pulse(self, net: "Network", von: str, is_high: bool):
        raise NotImplementedError("pulse")

    def reset(self):
        raise NotImplementedError("reset")


class ConjGate(Component):
    def __init__(self, name: str):
        self.memory: dict[str, bool] = {}
        self.name = name
        self.dbg = ""

    def pulse(self, net: "Network", von: str, is_high: bool):
        self.memory[von] = is_high
        if self.dbg and any(self.memory.values()):
            print("DBG:", self.dbg, self.memory)
            self.dbg = ""
        if all(self.memory.values()):
            net.pulse(self.name, False)
        else:
            net.pulse(self.name, True)

    def reset(self):
        for von in sorted(self.memory):
            self.memory[von] = False


class Broadcast(Component):
    def __init__(self, name: str):
        self.name = name

    def pulse(self, net: "Network", von: str, is_high: bool):
        net.pulse(self.name, is_high)

    def reset(self):
        pass


class FlipFlop(Component):
    def __init__(self, name: str):
        self.state = False
        self.name = name

    def pulse(self, net: "Network", von: str, is_high: bool):
        if is_high:
            return
        self.state = not self.state
        net.pulse(self.name, self.state)

    def reset(self):
        self.state = False


class Network:
    def __init__(self):
        self.pulse_queue: list[tuple[str, bool]] = []
        self.components: dict[str, Component] = {}
        self.connections: dict[str, list[str]] = {}

    def reset(self):
        self.pulse_queue = []
        for comp in self.components.values():
            comp.reset()

    def pulse(self, von: str, is_high: bool):
        self.pulse_queue.extend((von, zu, is_high) for zu in self.connections.get(von, []))

    def low_pulse_everything(self):
        all_components = sorted(self.components)
        for comp_name in all_components:
            for zu in self.connections.get(comp_name, []):
                if zu not in self.components:
                    continue
                self.components[zu].pulse(self, comp_name, False)
        self.reset()

    def add_component(self, comp: Component):
        self.components[comp.name] = comp

    def add_connection(self, von_name: str, zu_names: list[str]):
        self.connections[von_name] = zu_names

    def push_button(self) -> tuple[int, int]:
        rethigh, retlow = 0, 0
        self.pulse_queue.append(("button", "broadcaster", False))
        while self.pulse_queue:
            (von, zu, is_high) = self.pulse_queue.pop(0)
            if is_high:
                rethigh += 1
            else:
                retlow += 1
            if zu in self.components:
                self.components[zu].pulse(self, von, is_high)
        return (rethigh, retlow)


rules = aoc_util.get_data_lines(20)
network = Network()
for rule in rules:
    m1 = re.match(r"([%&]?)(\w+) *->", rule)
    name = m1.group(2)
    if not m1.group(1):
        comp = Broadcast(name)
    elif m1.group(1) == "%":
        comp = FlipFlop(name)
    elif m1.group(1) == "&":
        comp = ConjGate(name)
    network.add_component(comp)
for rule in rules:
    m1 = re.match(r"[%&]?(\w+) *-> *(.*)", rule)
    zu_list = list(re.split(r", *", m1.group(2)))
    name = m1.group(1)
    network.add_connection(name, zu_list)
network.low_pulse_everything()
network.reset()
print("Starting...")
hi, lo = 0, 0
for _ in range(1000):
    h, l = network.push_button()
    hi += h
    lo += l
print("separated", hi, lo)
print(hi * lo)

rxcomp = FlipFlop("rx")
network.add_component(rxcomp)
network.reset()
pushes = 0
pre_rx_names = [cname for (cname, val) in network.connections.items() if "rx" in val]
pre_rx_components = [network.components[cname] for cname in pre_rx_names]
while not rxcomp.state:
    pushes += 1
    for pre_rx in pre_rx_components:
        pre_rx.dbg = f"pushes({pushes})"
    network.push_button()
print(pushes)
