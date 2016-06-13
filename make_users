#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname make_users
-module(make_users).

-export([main/1]).
-export([make/0]).
-export([make/1]).

main([String]) ->
    Num = list_to_integer(String),
    make(Num).

make() ->
    make(100).

make(Num) ->
    io:format(user, "GUID,\"Employee ID\",\"User name\",\"First name\",\"Middle initial\",\"Last name\",\"Display name\",\"Preferred number\",\"Email address\",Extension,\"DID number\",Title,Password,\"User Type\",\"Client type access\",\"Default unit\",Department,Building,\"Role 1\",\"Role 2\",\"Role 3\",\"Role 4\",\"Role 5\",\"Role 6\",\"Role 7\",\"Role 8\",\"Role 9\",\"Role 10\",\"Role 11\",\"Role 12\",\"Auth Type\",\"LDAP User name\"~n", []),
    [make_emp(N) || N <- lists:seq(0, Num)].

make_emp(Num) ->
    ID = 2000 + 1000 - Num,
    IDList = integer_to_list(ID),
    FName = [$a | IDList],
    LName = [$z | IDList],
    EmpNo = FName,
    FullName = FName ++ " " ++ LName,
    Email = FName ++ "@voalte.com",
    Ext = 15000 + Num,
    Rest = "RN,1,user,\"voalte_one,voalte_messenger\",Oncology,Nursing,\"West Tower\",\"Default Roles,Oncology Charge Nurse\",,,,,,,,,,,,voalte,",
    io:format(user, ",~p,~s,~s,,~s,\"~s\",,~s,~p,,~s~n",
              [ID, EmpNo, FName, LName, FullName, Email, Ext, Rest]).
