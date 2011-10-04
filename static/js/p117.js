$(document).ready(function () {

    function rebuttonButtons () {
        $("#editButton").button().click(editButtonHandler);
    };

    var ajaxError = function (x,t,m) {
        alert('Server connection error');
    };

    function displaySelectedPage (pageId) {
        var res = "ok"
        $.ajax({
            type: "GET",
            url: "/mainpage/page",
            data: "pageId="+pageId,
            dataType: "html",
            error: function (x,t,m) {
                res = "fail";
                ajaxError(x,t,m);
            },
            success: function (ans) {
                $('#pageText').html(ans);
            },
        });
        return res;
    };

    function selectItem () {
        var pageId = this.getAttribute("data-pageid");
        var selectedItemId = $('#mainTree').attr("selectedItemId");
        if (pageId != selectedItemId) {
            var res = displaySelectedPage(pageId);
            if (res == "ok") {
                var selectedItemId = $('#mainTree').attr("selectedItemId");
                $('div.Content[data-pageid="'+selectedItemId+'"]').removeClass("SelectedItem");

                $('#mainTree').attr("selectedItemId", pageId);
                $('div.Content[data-pageid="'+pageId+'"]').addClass("SelectedItem");
            }
            else {
            }
        }
    };

    function editButtonHandler () {
        var selectedItemId = $('#mainTree').attr("selectedItemId");

        function submitPage () {
            var str = $("#editForm").serialize();
            str = str + "&submit=Submit&pageId="+selectedItemId;
            $.ajax({
                type: "POST",
                url: "/mainpage/editpage",
                data: str,
                dataType: "json",
                error: ajaxError,
                success: function (ans) {
                    if (ans == "ok") {
                        displaySelectedPage(selectedItemId);
                    }
                    else {
                    }
                },
            });

        };

        $.ajax({
            type: "GET",
            url: "/mainpage/editpage",
            data: "pageId="+selectedItemId,
            dataType: "html",
            error: ajaxError,
            success: function (ans) {
                $('#pageText').html(ans);
                $('#previewButton').button();
                $('#submitButton').button().click(submitPage);
            },
        });
    };


    $("div.Content").click(selectItem);

    rebuttonButtons();
})
