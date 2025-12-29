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
step(3, inference, rule_frames_305_total, frames_from_305_pages(3050)).
rule(rule_frames_305_total, multiplication_rule, [pages_with_305_frames(_), frames_per_page_305(_)], frames_from_305_pages(_)).
normalize(pages_with_305_frames(X), factor_0(X)).
normalize(frames_per_page_305(Y), factor_1(Y)).
normalize(frames_from_305_pages(Z), product(Z)).

step(4, given, pages_with_250_frames(7)).
step(5, given, frames_per_page_250(250)).
step(6, inference, rule_frames_250_total, frames_from_250_pages(1750)).
rule(rule_frames_250_total, multiplication_rule, [pages_with_250_frames(_), frames_per_page_250(_)], frames_from_250_pages(_)).
normalize(pages_with_250_frames(X), factor_0(X)).
normalize(frames_per_page_250(Y), factor_1(Y)).
normalize(frames_from_250_pages(Z), product(Z)).

step(7, given, total_pages(25)).
step(8, inference, rule_subtract_305_pages, pages_after_removing_305(15)).
rule(rule_subtract_305_pages, subtraction_rule, [total_pages(_), pages_with_305_frames(_)], pages_after_removing_305(_)).
normalize(total_pages(X), minuend(X)).
normalize(pages_with_305_frames(Y), subtrahend(Y)).
normalize(pages_after_removing_305(Z), difference(Z)).

step(9, inference, rule_subtract_250_pages, pages_with_average_frames(8)).
rule(rule_subtract_250_pages, subtraction_rule, [pages_after_removing_305(_), pages_with_250_frames(_)], pages_with_average_frames(_)).
normalize(pages_after_removing_305(X), minuend(X)).
normalize(pages_with_250_frames(Y), subtrahend(Y)).
normalize(pages_with_average_frames(Z), difference(Z)).

step(10, given, average_frames_per_page(280)).
step(11, inference, rule_frames_average_total, frames_from_average_pages(2240)).
rule(rule_frames_average_total, multiplication_rule, [pages_with_average_frames(_), average_frames_per_page(_)], frames_from_average_pages(_)).
normalize(pages_with_average_frames(X), factor_0(X)).
normalize(average_frames_per_page(Y), factor_1(Y)).
normalize(frames_from_average_pages(Z), product(Z)).

step(12, inference, rule_add_305_and_250, frames_from_305_and_250(4800)).
rule(rule_add_305_and_250, addition_rule, [frames_from_305_pages(_), frames_from_250_pages(_)], frames_from_305_and_250(_)).
normalize(frames_from_305_pages(X), addend_0(X)).
normalize(frames_from_250_pages(Y), addend_1(Y)).
normalize(frames_from_305_and_250(Z), sum(Z)).

step(13, inference, rule_total_frames, total_frames(7040)).
rule(rule_total_frames, addition_rule, [frames_from_305_and_250(_), frames_from_average_pages(_)], total_frames(_)).
normalize(frames_from_305_and_250(X), addend_0(X)).
normalize(frames_from_average_pages(Y), addend_1(Y)).
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
