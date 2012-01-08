$(document).ready(function () {

    function rebuttonButtons () {
        $("#editButton").button().click(editButtonHandler);
        $("#addButton").button().click(addButtonHandler);
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
                rebuttonButtons();
            },
        });
        return res;
    };

    function getPathForItemDiv (divElement) {
        var pathBegin = divElement.getAttribute("data-pageid");

        if ($(divElement).parent().hasClass("IsRoot")) {
            return pathBegin
        }
        else {
            parentDivElement = $(divElement).parent().parent().siblings("div.Content")[0];
            nextId = parentDivElement.getAttribute("data-pageid");
            return (getPathForItemDiv (parentDivElement) + "_" + pathBegin)
        }
    }

    function getItemDivForPath (path) {
        pathElems = path.split("_");
        rootElemId = pathElems.shift();
        // 1. Find root element
        rootElem = $("li.IsRoot > div.Content[data-pageid="+rootElemId+"]")[0];

        // 2. Find other elements (divs)
        nextElem = rootElem;
        for (i in pathElems) {
            // 2.1. Find "Container"
            container = $(nextElem).siblings("ul.Container")[0];

            // 2.2 Find a li element that has a child - a div with Content class and data-pageid=pathElems[i] attribute
            nextDiv = null;
            lis = $(container).children();
            for (j=0; j < lis.length; j = j + 1) {
                a = $(lis[j]).children("div.Content[data-pageid="+pathElems[i]+"]");
                if (a.length > 0) {
                    nextDiv = a[0];
                }
            }
            if (nextDiv == null) {
                alert("there is no path " + path);
            }
            else {
                nextElem = nextDiv;
            }
        }
        // 3. The last element in the list is the div we need
        return nextElem;
    }

    function clickTreeItem () {
        var pageId = this.getAttribute("data-pageid");

        path = getPathForItemDiv(this);

        //debug
        item = getItemDivForPath(path);
        console.log(item);

        selectTreeItem(pageId);
    }

    function selectTreeItem (pageId) {
        var selectedItemId = $('#mainTree').attr("data-selectedItemId");
        if (pageId != selectedItemId) {
            var selectedItemId = $('#mainTree').attr("data-selectedItemId");
            $('div.Content[data-pageid="'+selectedItemId+'"]').removeClass("SelectedItem");

            $('#mainTree').attr("data-selectedItemId", pageId);
            $('div.Content[data-pageid="'+pageId+'"]').addClass("SelectedItem");
            // TODO: раскрыть всю ветвь

            var res = displaySelectedPage(pageId);
        }
    };

    function reloadTree (pageToSelectId) {
        var pageId = pageToSelectId || $('#mainTree').attr("data-selectedItemId");
        var predicateId = $('#mainTree').attr("data-predicateid");
        saveTreeState();
        $('div#treeContainer').load('/mainpage/tree?predicateId='+predicateId, function () {
            restoreTreeState();
            selectTreeItem(pageId);
            $("div.Content").click(clickTreeItem);
        });
    }

    function saveTreeState () {
        var getIdState = function (index, elemen) {
            var id = $(element).attr("data-pageid");
            var liElem = $(element).parent();
            var state;
            if ($(liElem).hasClass('ExpandOpen')) {
                state = 'open';
            }
            if ($(liElem).hasClass('ExpandClosed')) {
                state = 'closed';
            }
            if ($(liElem).hasClass('ExpandLeaf')) {
                state = 'leaf';
            }
            return {id: id, state: state};
        };
        var itemsContent = $('#mainTree').find('div.Content');
        treeState = itemsContent.map(getIdState);
    };

    function restoreTreeState () {
        var restoreIdState = function (index, element) {
            var id = $(element).attr('data-pageid');
            var liElem = $(element).parent();
            for (var i=0; i < treeState.length; i++){
                if (treeState[i].id == id) {
                    var state = treeState[i].state;
                    if (state == "open") {
                        $(liElem).removeClass("ExpandClosed");
                        $(liElem).addClass("ExpandOpen");
                    }
                    if (state == "closed") {
                        $(liElem).removeClass("ExpandOpen");
                        $(liElem).addClass("ExpandClosed");
                    }
                }
            };
        };

        var itemsContent = $('#mainTree').find('div.Content');
        itemsContent.map(restoreIdState);
    };

    function editButtonHandler () {
        var selectedItemId = $('#mainTree').attr("data-selectedItemId");

        function submitPage () {
            var str = $("#editForm").serialize();
            // Save 'title' field to change it in tree
            var titleParam = $('#editForm > input[name="title"]').serializeArray();
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
                        $('div.Content[data-pageid="'+selectedItemId+'"] > span.ItemText').html(titleParam[0].value);
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

    function addButtonHandler () {
        var selectedItemId = $('#mainTree').attr("data-selectedItemId");
        var predicateId = $('#mainTree').attr("data-predicateId");

        // Get id of page parent in the tree. It's used when create new page at the same level as selected page.
        var parentSelectedItemId = $($($($('div.Content[data-pageid="'+selectedItemId+'"]').parent()).parent()).prev()).attr("data-pageid") || -1;

        function submitPage () {
            var str = $("#addForm").serialize();
            str = str + "&submit=Submit&pageId="+selectedItemId+"&predicateId="+predicateId+"&parentId="+parentSelectedItemId;
            $.ajax({
                type: "POST",
                url: "/mainpage/addpage",
                data: str,
                dataType: "json",
                error: ajaxError,
                success: function (ans) {
                    if (ans[0] == "ok") {
                        var pageId = ans[1];
                        // reload tree
                        reloadTree(pageId);
                    }
                    else
                    {
                        alert('Ошибка добавления страницы:'+ans);
                    }
                },
            });
        };

        $.ajax({
            type: "GET",
            url: "/mainpage/addpage",
            data: "pageId="+selectedItemId+"&predicateId="+predicateId,
            dataType: "html",
            error: ajaxError,
            success: function (ans) {
                $("#pageText").html(ans);
                $('#previewButton').button();
                $('#submitButton').button().click(submitPage);
            }
        });
    };

    function changePredicateSelectHandler () {
        var newPredicate = $('#predicateSelect').val();
        $('#mainTree').attr("data-predicateid", newPredicate);
        reloadTree();
    };

    function selectFirstItem() {
        var pageToSelect = $($($('span.ItemText').first()).parent()).attr("data-pageid");
        if (pageToSelect) {
            selectTreeItem(pageToSelect);
        }
        else {
            alert("there are no pages in the predicate yet");
        }
    };

    //^^^^^^^^^^^^^ Only functions definitions ^^^^^^^^^^^^^^^^^^^^^

    var treeState = 0;
    $("div.Content").click(clickTreeItem);
    selectFirstItem();

    $('#predicateSelect').change(changePredicateSelectHandler);


    $('#testButton').button().click( function () {
        reloadTree();
    });
})
