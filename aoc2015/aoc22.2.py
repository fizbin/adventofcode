import collections

State = collections.namedtuple(
    'State',
    ['player_hp',
     'boss_hp',
     'mana',
     'shield_timer',
     'poison_timer',
     'recharge_timer',
     'total_spend'])


def timer_effects(state):
    if state.poison_timer > 0:
        state = state._replace(boss_hp=state.boss_hp - 3)
    if state.recharge_timer > 0:
        state = state._replace(mana=state.mana + 101)
    return state._replace(
        shield_timer=max(0, state.shield_timer - 1),
        poison_timer=max(0, state.poison_timer - 1),
        recharge_timer=max(0, state.recharge_timer - 1))


def spell(state, n):
    if n == 0:
        return state._replace(
            mana=state.mana - 53,
            boss_hp=state.boss_hp - 4,
            total_spend=state.total_spend + 53)
    elif n == 1:
        return state._replace(
            mana=state.mana - 73,
            player_hp=state.player_hp + 2,
            boss_hp=state.boss_hp - 2,
            total_spend=state.total_spend + 73)
    elif n == 2 and state.shield_timer == 0:
        return state._replace(
            mana=state.mana - 113,
            shield_timer=6,
            total_spend=state.total_spend + 113)
    elif n == 3 and state.poison_timer == 0:
        return state._replace(
            mana=state.mana - 173,
            poison_timer=6,
            total_spend=state.total_spend + 173)
    elif n == 4 and state.recharge_timer == 0:
        return state._replace(
            mana=state.mana - 229,
            recharge_timer=5,
            total_spend=state.total_spend + 229)
    return False


def boss_turn(state):
    has_shield = (state.shield_timer > 0)
    state = timer_effects(state)
    if state.boss_hp <= 0:
        return state
    if has_shield:
        state = state._replace(player_hp=state.player_hp-3)
    else:
        state = state._replace(player_hp=state.player_hp-10)
    if state.player_hp <= 0:
        return False
    return state


def player_turn(state, spell_n):
    state = timer_effects(state)
    if state.boss_hp <= 0:
        return state
    state = spell(state, spell_n)
    if isinstance(state, bool):
        return state
    if state.mana < 0:
        return False
    return state

mincost = 9999999999
states = set([(State(
    player_hp=50,
    boss_hp=71,
    mana=500,
    shield_timer=0,
    poison_timer=0,
    recharge_timer=0,
    total_spend=0), 0)])

while states:
    (state, next_spell) = states.pop()
    if next_spell < 4:
        states.add((state, next_spell + 1))
    if state.player_hp <= 1:
        continue
    state = state._replace(player_hp=state.player_hp - 1)
    state = player_turn(state, next_spell)
    if state is False:
        continue
    if state.boss_hp <= 0:
        mincost = min(mincost, state.total_spend)
        continue
    state = boss_turn(state)
    if state is False:
        continue
    if state.boss_hp <= 0:
        mincost = min(mincost, state.total_spend)
        continue
    states.add((state, 0))

print(mincost)
