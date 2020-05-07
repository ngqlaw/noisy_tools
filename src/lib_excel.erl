%%%-------------------------------------------------------------------
%%% @author ngq <ngq_scut@126.com>
%%% @doc
%%% Excel parser
%%% @end
%%%-------------------------------------------------------------------
-module(lib_excel).

-export([open/1, open_content/1]).

-include("excel.hrl").
-include_lib("xmerl/include/xmerl.hrl").

open(File) ->
    {ok, ContentBin} = file:read_file(File),
    open_content(ContentBin).

open_content(Content) ->
    {ok, ExcelData} = zip:unzip(Content, [memory]),

    % parse sheets info
    WorkbookBinary = proplists:get_value("xl/workbook.xml", ExcelData),
    {WorkbookDoc, _} = xmerl_scan:string(erlang:binary_to_list(WorkbookBinary)),
    [#xmlElement{content = SheetsXML}] = xmerl_xpath:string("/workbook/sheets", WorkbookDoc),
    SheetInfos = [new_excel_sheet(SheetXML)||SheetXML <- SheetsXML],

    % prase string table info
    SharedStringsBinary = proplists:get_value("xl/sharedStrings.xml", ExcelData),
    {SharedStringsDoc, _} = xmerl_scan:string(erlang:binary_to_list(SharedStringsBinary)),
    SharedStringXML = xmerl_xpath:string("/sst/si/t", SharedStringsDoc),
    {ok, StringTable} = new_excel_string_table(SharedStringXML),

    % load sheets data
    {ok, lists:foldr(
    fun(SheetInfo = #excel_sheet{id = SheetId}, AccIn) ->
        SheetDataFile = lists:concat(["xl/worksheets/sheet", SheetId, ".xml"]),
        case proplists:get_value(SheetDataFile, ExcelData) of
            undefined -> AccIn;
            SheetDataBinary ->
                {SheetDataDoc, _} = xmerl_scan:string(erlang:binary_to_list(SheetDataBinary)),
                [#xmlElement{content = RowsXML}] = xmerl_xpath:string("/worksheet/sheetData", SheetDataDoc),
                Rows = [new_excel_row(RowXML, StringTable) || RowXML<-RowsXML],
                [SheetInfo#excel_sheet{content = lists:append(Rows)}|AccIn]
        end
    end, [], SheetInfos)}.

new_excel_string_table(SharedStringXML) ->
    new_excel_string_table(SharedStringXML, dict:new(), 0).

new_excel_string_table([#xmlElement{content = [#xmlText{value = Value}|_]}|T], StringTable, Index) ->
    NewStringTable = dict:store(Index, Value, StringTable),
    new_excel_string_table(T, NewStringTable, Index + 1);
new_excel_string_table([], StringTable, _Index) -> {ok, StringTable}.

new_excel_sheet(#xmlElement{attributes = Attrs}) ->
    {value, #xmlAttribute{value = SheetName}} = lists:keysearch(name, #xmlAttribute.name, Attrs),
    {value, #xmlAttribute{value = SheetIdStr}} = lists:keysearch(sheetId, #xmlAttribute.name, Attrs),
    #excel_sheet{id = SheetIdStr, name = SheetName}.

new_excel_row(#xmlElement{content = CellsXML}, StringTable) ->
    [new_excel_cell(CellXML, StringTable)|| CellXML <- CellsXML].

new_excel_cell(#xmlElement{
        attributes = Attrs,
        content = Content
    }, StringTable) ->
    {value, #xmlElement{
        content = [#xmlText{parents = CellInfo, value = V}]
    }} = lists:keysearch(v, #xmlElement.name, Content),
    Row = proplists:get_value(row, CellInfo),
    Cell = proplists:get_value(c, CellInfo),
    Value =
        case lists:keysearch(t, #xmlAttribute.name, Attrs) of
            {value, #xmlAttribute{value = "s"}} ->
                dict:fetch(list_to_integer(V), StringTable);
            {value, _} ->
                V;
            false ->
                V
        end,
    #excel_cell{r = Row, c = Cell, v = Value}.
