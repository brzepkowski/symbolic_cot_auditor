% -------------------------
% Steps (the CoT trace)
% -------------------------

step(1, given, apples_total(10)).
step(2, given, apples_eaten(3)).
step(3, inference, rule_remaining_apples, apples_remaining(7)).

normalize(apples_total(T), total(T)).
normalize(apples_eaten(E), consumed(E)).
normalize(apples_remaining(R), remaining(R)).

% -------------------------
% Rule schemas
% -------------------------

rule_schema(subtraction_rule,
    [total(_), consumed(_)],
    remaining(_)).

rule(rule_remaining_apples,
     subtraction_rule,
     [apples_total(T), apples_eaten(E)],
     apples_remaining(R)).

% -------------------------
% Derivability
% -------------------------

available(Fact, Step) :-
    step(S, inference, RuleName, Fact),
    S < Step,
    rule(RuleName, _, Premises, Fact),
    forall(member(P, Premises), available(P, S)).

available(Fact, Step) :-
    step(S, given, Fact),
    S < Step.

% -------------------------
% Correctness
% -------------------------

:- use_module(library(clpfd)).

valid_schema(subtraction_rule,
    [total(T), consumed(E)],
    remaining(R)) :-
    T #>= 0,
    E #>= 0,
    R #>= 0,
    R #= T - E.

% -------------------------
% Validators
% -------------------------

valid_rule(RuleName, Premises, Conclusion) :-
    rule(RuleName, Schema, Premises, Conclusion),
    maplist(normalize, Premises, NormPremises),
    normalize(Conclusion, NormConclusion),
    valid_schema(Schema, NormPremises, NormConclusion).

% Below bind_premises() tries to match each premise (required input) with an actual available fact,
% ensuring that all the premises needed for a rule can be "filled in" with real data.
bind_premises([], _).
bind_premises([P|Ps], StepID) :-  % split Premises into first premise `P` and remaining premises `Ps`
    available(Fact, StepID),  % search for any Fact available at StepID
    Fact = P,  % try to unify Fact with P
    bind_premises(Ps, StepID).

valid_step(StepID) :-
    step(StepID, inference, RuleName, StepConclusion),
    rule(RuleName, _, Premises, RuleConclusion),
    StepConclusion = RuleConclusion,
    bind_premises(Premises, StepID),
    valid_rule(RuleName, Premises, StepConclusion),
    forall(member(P, Premises), available(P, StepID)).

valid_conclusion(StepID) :-
    step(StepID, conclusion, Fact),
    available(Fact, StepID).

% -------------------------
% Audit the entire CoT
% -------------------------

audit :-
    forall(step(S, inference, _ , _), valid_step(S)),
    forall(step(S, conclusion, _), valid_conclusion(S)).
