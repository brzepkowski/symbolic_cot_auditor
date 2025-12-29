:- discontiguous step/3.
:- discontiguous step/4.
:- discontiguous rule/4.
:- discontiguous normalize/2.
:- discontiguous valid_schema/3.

% -------------------------
% Steps (the CoT trace)
% -------------------------

step(1, given, pages_with_305_frames(10)).
step(2, given, frames_per_page_305(305)).
step(3, inference, rule_frames_305_pages, total_frames_305_pages(3050)).
rule(rule_frames_305_pages, multiplication_rule, [pages_with_305_frames(_), frames_per_page_305(_)], total_frames_305_pages(_)).
normalize(pages_with_305_frames(X), factor_0(X)).
normalize(frames_per_page_305(Y), factor_1(Y)).
normalize(total_frames_305_pages(Z), product(Z)).

step(4, given, pages_with_250_frames(7)).
step(5, given, frames_per_page_250(250)).
step(6, inference, rule_frames_250_pages, total_frames_250_pages(1750)).
rule(rule_frames_250_pages, multiplication_rule, [pages_with_250_frames(_), frames_per_page_250(_)], total_frames_250_pages(_)).
normalize(pages_with_250_frames(X), factor_0(X)).
normalize(frames_per_page_250(Y), factor_1(Y)).
normalize(total_frames_250_pages(Z), product(Z)).

step(7, given_without_justification, remaining_pages(8)).  % Developer note: LLM added this line instead of "step(7, given, remaining_pages(8))."!
step(8, given, average_frames_per_page(280)).
step(9, inference, rule_frames_remaining_pages, total_frames_remaining_pages(2240)).
rule(rule_frames_remaining_pages, multiplication_rule, [remaining_pages(_), average_frames_per_page(_)], total_frames_remaining_pages(_)).
normalize(remaining_pages(X), factor_0(X)).
normalize(average_frames_per_page(Y), factor_1(Y)).
normalize(total_frames_remaining_pages(Z), product(Z)).

step(10, inference, rule_sum_305_and_250, partial_sum_305_250(4800)).
rule(rule_sum_305_and_250, addition_rule, [total_frames_305_pages(_), total_frames_250_pages(_)], partial_sum_305_250(_)).
normalize(total_frames_305_pages(X), addend_0(X)).
normalize(total_frames_250_pages(Y), addend_1(Y)).
normalize(partial_sum_305_250(Z), sum(Z)).

step(11, inference, rule_total_frames, total_frames(7040)).
rule(rule_total_frames, addition_rule, [partial_sum_305_250(_), total_frames_remaining_pages(_)], total_frames(_)).
normalize(partial_sum_305_250(X), addend_0(X)).
normalize(total_frames_remaining_pages(Y), addend_1(Y)).
normalize(total_frames(Z), sum(Z)).

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
    [minuend(X), subtrahend(Y)],
    difference(Z)) :-
    Z #= X - Y.

valid_schema(addition_rule,
    [addend_0(X), addend_1(Y)],
    sum(Z)) :-
    Z #= X + Y.

valid_schema(multiplication_rule,
    [factor_0(X), factor_1(Y)],
    product(Z)) :-
    Z #= X * Y.

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

% -------------------------
% Audit the entire CoT
% -------------------------

audit :-
    forall( 
        step(S, inference, _, _),
        (valid_step(S) -> 
            format('✓ Step ~w (inference) is valid~n', [S])
        ;
            format('✗ Step ~w (inference) FAILED~n', [S]),
            fail
        )
    ).
