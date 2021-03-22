from __future__ import annotations

from collections import defaultdict
import tatsu.grammars  # pylint: disable=R0401
from typing import Callable

# Based on https://github.com/ncellar/autumn_v1/


# Returns the correct Rule instance for a RuleRef
def follow(node, rule_dict):
    if isinstance(node, tatsu.grammars.RuleRef):
        return rule_dict.get(node.name, node)
    else:
        return node


class Nullable(object):
    def __init__(self, children, resolved=False, nullable=False):
        self.resolved = resolved
        self.nullable = nullable
        self.children = children

    def resolve(self, node, rule_dict):
        pass

    def resolve_with(self, n):
        self.resolved = True
        self.nullable = n
        self.children = None  # No longer needed

    all: Callable
    any: Callable
    of: Callable
    no: Callable
    yes: Callable


class _All(Nullable):
    def resolve(self, node, rule_dict):
        unresolved = []
        for c in self.children:
            c = follow(c, rule_dict)
            n = c._nullability
            if n.resolved:
                if not n.nullable:
                    # Not nullable if any is not nullable
                    self.resolve_with(False)
                    return
            else:
                unresolved.append(c)
        if not unresolved:
            # Nullable if all are nullable
            self.resolve_with(True)
        else:
            # Otherwise still unresolved
            self.children = unresolved


class _Any(Nullable):
    def resolve(self, node, rule_dict):
        # Inverse of All
        unresolved = []
        for c in self.children:
            c = follow(c, rule_dict)
            n = c._nullability
            if n.resolved:
                if n.nullable:
                    self.resolve_with(True)
                    return
            else:
                unresolved.append(c)
        if not unresolved:
            self.resolve_with(False)
        else:
            self.children = unresolved


class _Single(Nullable):
    def resolve(self, node, rule_dict):
        n = follow(self.children[0], rule_dict)._nullability
        if not n.resolved:
            return
        self.resolve_with(n.nullable)


Nullable.all = _All     # Nullable if all children are nullable
Nullable.any = _Any     # Nullable if one child is nullable
Nullable.of = lambda child: _Single([child])       # Nullable if the only child is nullable
Nullable.no = lambda: Nullable(None, True, False)  # Not nullable
Nullable.yes = lambda: Nullable(None, True, True)  # Nullable


def resolve_nullability(grammar, rule_dict):
    dependants = defaultdict(list)
    visited = set()     # To prevent infinite recursion

    def walk(model):    # TODO Write a walker for this?
        if model in visited:
            return
        visited.add(model)

        for child in model.children_list():
            child = follow(child, rule_dict)
            walk(child)

        resolve(model)

    def resolve(model):
        nullability = model._nullability
        if not nullability.resolved:
            nullability.resolve(model, rule_dict)
            if nullability.resolved:
                for dependant in dependants[model]:
                    n = dependant._nullability
                    if not n.resolved:
                        resolve(dependant)  # Resolve nodes that depend on this one
            else:
                for n in nullability.children:
                    dependants[n].append(model)

    walk(grammar)

# This breaks left recursive cycles by tagging
# left recursive rules


def find_left_recursion(grammar):
    rule_dict = {rule.name: rule for rule in grammar.rules}  # Required to resolve rule references

    # First we need to resolve nullable rules
    resolve_nullability(grammar, rule_dict)

    # Traversable state
    FIRST = 0
    CUTOFF = 1
    VISITED = 2

    state = defaultdict(lambda: FIRST)
    stack_depth = [0]  # nonlocal workaround 2.7
    stack_positions = {}
    lr_stack_positions = [-1]

    def walk(model):
        if state[model] == FIRST:
            state[model] = CUTOFF
        else:
            return

        # beforeNode
        leftrec = isinstance(model, tatsu.grammars.Rule) and model._is_leftrec
        if leftrec:
            lr_stack_positions.append(stack_depth[0])

        stack_positions[model] = stack_depth[0]
        stack_depth[0] += 1

        for child in model.at_same_pos(rule_dict):
            child = follow(child, rule_dict)
            walk(child)
            # afterEdge
            if state[child] == CUTOFF:  # active cycle
                if stack_positions[child] > lr_stack_positions[-1]:
                    # turn off memoization for all rules that were involved in this cycle
                    for rule in stack_positions:
                        if isinstance(rule, tatsu.grammars.Rule):
                            rule.is_memoizable = False

                    child._is_leftrec = True

        # afterNode
        if leftrec:
            lr_stack_positions.pop()

        del stack_positions[model]
        stack_depth[0] -= 1

        state[model] = VISITED

    for rule in grammar.children_list():
        walk(rule)

    # print()
    # for rule in grammar.children_list():
    #    print(rule)
    #    if rule._is_leftrec: print("-> Leftrec")
    #    if rule.is_nullable(): print("-> Nullable")

    return
