:- discontiguous step/3.
:- discontiguous step/4.
:- discontiguous rule/4.
:- discontiguous normalize/2.
:- discontiguous valid_schema/3.

% -------------------------
% Steps (the CoT trace)
% -------------------------

step(1, given, total_pages(25)).
step(2, given, avg_frames_per_page(280)).

step(3, given, pages_high_frames(10)).
step(4, given, frames_high(305)).

step(5, inference, rule_high_pages_frames, total_high_frames(3050)).
rule(rule_high_pages_frames, multiplication_rule,
     [pages_high_frames(_), frames_high(_)],
     total_high_frames(_)).
normalize(pages_high_frames(X), factor_0(X)).
normalize(frames_high(Y), factor_1(Y)).
normalize(total_high_frames(Z), product(Z)).

step(6, given, pages_low_frames(7)).
step(7, given, frames_low(250)).

step(8, inference, rule_low_pages_frames, total_low_frames(1750)).
rule(rule_low_pages_frames, multiplication_rule,
     [pages_low_frames(_), frames_low(_)],
     total_low_frames(_)).
normalize(pages_low_frames(X), factor_0(X)).
normalize(frames_low(Y), factor_1(Y)).
normalize(total_low_frames(Z), product(Z)).

step(9, inference, rule_remaining_pages_step1, pages_after_high(15)).
rule(rule_remaining_pages_step1, subtraction_rule,
     [total_pages(_), pages_high_frames(_)],
     pages_after_high(_)).
normalize(total_pages(X), minuend(X)).
normalize(pages_high_frames(Y), subtrahend(Y)).
normalize(pages_after_high(Z), difference(Z)).

step(10, inference, rule_remaining_pages_step2, pages_average(8)).
rule(rule_remaining_pages_step2, subtraction_rule,
     [pages_after_high(_), pages_low_frames(_)],
     pages_average(_)).
normalize(pages_after_high(X), minuend(X)).
normalize(pages_low_frames(Y), subtrahend(Y)).
normalize(pages_average(Z), difference(Z)).

step(11, inference, rule_average_pages_frames, total_average_frames(2240)).
rule(rule_average_pages_frames, multiplication_rule,
     [pages_average(_), avg_frames_per_page(_)],
     total_average_frames(_)).
normalize(pages_average(X), factor_0(X)).
normalize(avg_frames_per_page(Y), factor_1(Y)).
normalize(total_average_frames(Z), product(Z)).

step(12, inference, rule_sum_frames_step1, partial_total_frames(4800)).
rule(rule_sum_frames_step1, addition_rule,
     [total_high_frames(_), total_low_frames(_)],
     partial_total_frames(_)).
normalize(total_high_frames(X), addend_0(X)).
normalize(total_low_frames(Y), addend_1(Y)).
normalize(partial_total_frames(Z), sum(Z)).

step(13, inference, rule_sum_frames_step2, total_frames(7040)).
rule(rule_sum_frames_step2, addition_rule,
     [partial_total_frames(_), total_average_frames(_)],
     total_frames(_)).
normalize(partial_total_frames(X), addend_0(X)).
normalize(total_average_frames(Y), addend_1(Y)).
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
