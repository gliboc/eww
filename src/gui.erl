-module(gui).

-include_lib("wx/include/wx.hrl").
-export([init/1]).

init(Platform) ->
    %catch_exception (true),
    %My_wx_dir = code:lib_dir(wx),
    %rr (My_wx_dir ++ "/include/wx,hrl"),
    %rr (My_wx_dir ++ "/src/wxe,hrl"),
    Wx=wx:new(),
    F=wxFrame:new(Wx, -1, "eww interface"),
    wxFrame:createStatusBar(F), 
    wxFrame:setStatusText(F, "Creator gliboc"),
    MenuBar = wxMenuBar:new(),
    wxFrame:setMenuBar(F, MenuBar),
    wxFrame:getMenuBar(F),

    FileMn = wxMenu:new(),
    wxMenuBar:append (MenuBar, FileMn, "&File"),
    Quit = wxMenuItem:new ([{id,400},{text, "&Quit"}]),
    wxMenu:append (FileMn, Quit),

    ConnectMn = wxMenu:new(),
    wxMenuBar:append (MenuBar, ConnectMn, "&Connect"),
    New = wxMenuItem:new ([{id,450},{text, "&New"}]),
    Join = wxMenuItem:new ([{id,451},{text, "&Join"}]),
    wxMenu:append (ConnectMn, New),
    wxMenu:append (ConnectMn, Join),

    HelpMn = wxMenu:new(),
    wxMenuBar:append (MenuBar, HelpMn, "&Help"),
    About = wxMenuItem:new ([{id,500},{text,"About"}]),
    wxMenu:append (HelpMn, About),

    %wxFrame:connect (F, close_window),
    %Ev = fun() -> receive E->E after 0 -> empty end end,

    Contact = wxMessageDialog:new (F, "This app was created by gliboc,\nContact: guilduboc@gmail,com"),
    NewDialog = wxMessageDialog:new (F, "Start a new platform at this address"),
    JoinDialog = wxMessageDialog:new (F, "Join a new platform using its IP"),

    Dialog = fun (Wx_obj,_) ->
         if Wx_obj#wx.id == 500 ->
                wxMessageDialog:showModal (Contact);
            Wx_obj#wx.id == 450 ->
                wxMessageDialog:showModal (NewDialog);
            Wx_obj#wx.id == 451 ->
                wxMessageDialog:showModal (JoinDialog);
            Wx_obj#wx.id == 400 ->
                wxFrame:destroy(F)
        end
    end,

    wxFrame:connect (F, command_menu_selected, [{callback, Dialog}]),

    % Buttons
    
    Button_handle = fun (Wx_obj,_) ->
        if Wx_obj#wx.id == 101 ->
               client:ping(Platform)
        end
    end,

    B101  = wxButton:new(F, 101, [{label, "&Ping"}]),
    %B102  = wxButton:new(F, 102, [{label, "&Button2"}]), 
    wxFrame:connect (F, command_button_clicked, [{callback, Button_handle}]),

    wxFrame:show (F).
