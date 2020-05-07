-ifndef(EXCEL_H).

-define(EXCEL_H, true).

%% 页
-record(excel_sheet, {
    id = 0,
    name = "",
    content = []
}).

%% 单元格
-record(excel_cell, {
    r, %% 行
    c, %% 列
    v  %% 数据
}).

-endif.