:- discontiguous step/3.
:- discontiguous step/4.
:- discontiguous rule/4.
:- discontiguous normalize/2.
:- discontiguous valid_schema/3.

% -------------------------
% Steps (the CoT trace)
% -------------------------

step(1, given, average_frames_per_page(280)).
step(2, given, total_pages(25)).

step(3, given, pages_with_305_frames(10)).
step(4, given, frames_per_305_page(305)).

step(5, inference, rule_frames_305_total, frames_305_total(3050)).
rule(rule_frames_305_total, multiplication_rule, [pages_with_305_frames(_), frames_per_305_page(_)], frames_305_total(_)).
normalize(pages_with_305_frames(X), factor_0(X)).
normalize(frames_per_305_page(Y), factor_1(Y)).
normalize(frames_305_total(Z), product(Z)).

step(6, given, pages_with_250_frames(7)).
step(7, given, frames_per_250_page(250)).

step(8, inference, rule_frames_250_total, frames_250_total(1750)).
rule(rule_frames_250_total, multiplication_rule, [pages_with_250_frames(_), frames_per_250_page(_)], frames_250_total(_)).
normalize(pages_with_250_frames(X), factor_0(X)).
normalize(frames_per_250_page(Y), factor_1(Y)).
normalize(frames_250_total(Z), product(Z)).

step(9, inference, rule_remaining_after_305, pages_after_305(15)).
rule(rule_remaining_after_305, subtraction_rule, [total_pages(_), pages_with_305_frames(_)], pages_after_305(_)).
normalize(total_pages(X), minuend(X)).
normalize(pages_with_305_frames(Y), subtrahend(Y)).
normalize(pages_after_305(Z), difference(Z)).

step(10, inference, rule_remaining_pages, remaining_pages(3)).
rule(rule_remaining_pages, subtraction_rule, [pages_after_305(_), pages_with_250_frames(_)], remaining_pages(_)).
normalize(pages_after_305(X), minuend(X)).
normalize(pages_with_250_frames(Y), subtrahend(Y)).
normalize(remaining_pages(Z), difference(Z)).

step(11, inference, rule_frames_average_total, frames_average_total(2240)).
rule(rule_frames_average_total, stated_multiplication_rule, [remaining_pages(_), average_frames_per_page(_)], frames_average_total(_)).
normalize(remaining_pages(X), factor_0(X)).
normalize(average_frames_per_page(Y), factor_1(Y)).
normalize(frames_average_total(Z), product(Z)).

step(12, inference, rule_sum_partial_frames, partial_frames_sum(4800)).
rule(rule_sum_partial_frames, addition_rule, [frames_305_total(_), frames_250_total(_)], partial_frames_sum(_)).
normalize(frames_305_total(X), addend_0(X)).
normalize(frames_250_total(Y), addend_1(Y)).
normalize(partial_frames_sum(Z), sum(Z)).

step(13, inference, rule_total_frames, total_frames(7040)).
rule(rule_total_frames, addition_rule, [partial_frames_sum(_), frames_average_total(_)], total_frames(_)).
normalize(partial_frames_sum(X), addend_0(X)).
normalize(frames_average_total(Y), addend_1(Y)).
normalize(total_frames(Z), sum(Z)).

valid_schema(stated_multiplication_rule,
    [factor_0(_), factor_1(_)],
    product(_)).

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

% valid_conclusion(StepID) :-
%     step(StepID, conclusion, Fact),
%     available(Fact, StepID).

% -------------------------
% Audit the entire CoT
% -------------------------

% audit :-
%     forall(step(S, inference, _ , _), valid_step(S)),
%     forall(step(S, conclusion, _), valid_conclusion(S)).

% audit :-
%     forall(
%         step(S, inference, _, _),
%         (valid_step(S) -> 
%             format('✓ Step ~w (inference) is valid~n', [S])
%         ;
%             format('✗ Step ~w (inference) FAILED~n', [S]),
%             fail
%         )
%     ),
%     forall(
%         step(S, conclusion, _),
%         (valid_conclusion(S) ->
%             format('✓ Step ~w (conclusion) is valid~n', [S])
%         ;
%             format('✗ Step ~w (conclusion) FAILED~n', [S]),
%             fail
%         )
%     ).

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
