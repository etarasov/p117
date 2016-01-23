$(document).ready(function () {

    function rebuttonButtons () {
        $("#editButton").button().click(editButtonHandler);
        $("#addButton").button().click(addButtonHandler);
    };

    var ajaxError = function (x,t,m) {
        alert('Server connection error: '+ t + m);
    };

    function displaySelectedPage (pageId) {
        var res = "ok";
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
                $('#pageText').data('page-id', pageId);
                rebuttonButtons();
            }
        });
        return res;
    };

    function getPathForNode (node) {
        var pathBegin = node.data.pageId;
        var res, parentNode;

        if (node.getLevel() == 0) {
            alert("Trying to get path for root node");
        }

        if (node.getLevel() == 1) {
            res = pathBegin;
        }
        else {
            parentNode = node.getParent();
            res = getPathForNode(parentNode) + ";" + pathBegin;
        }

        return res;
    }

    function getNodeForPath(root, path) {
        var res = null;
        if (path == "") {
            res = root;
        }
        else {
            var pathElems = path.split(";");
            var rootElem = pathElems.shift();

            var children = root.getChildren() || [];
            for (var i=0; i < children.length; i++) {
                if (children[i].data.pageId == rootElem) {
                    res = getNodeForPath(children[i], pathElems.join(";"));
                    break;
                }
            }
        }

        return res;
    }

    function editButtonHandler () {
        var selectedItemId = $('#pageText').data('page-id');

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
                        $('#tree1').dynatree("getTree").reload();
                        $('#tree2').dynatree("getTree").reload();
                    }
                    else {
                        alert(ans);
                    }
                }
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
            }
        });
    };

    function addButtonHandler () {
        var predicateId = $('#treeContainer1').attr("data-predicateId");

        var path = $('#treeContainer1').attr("data-selectedpath");
        var selectedItemId = path.split(";")[path.split(";").length - 1];

        // Get id of page parent in the tree. It's used when create new page at the same level as selected page.
        // TODO: it's a code for previous js tree widget
        //var parentSelectedItemId = $($($($('div.Content[data-pageid="'+selectedItemId+'"]').parent()).parent()).prev()).attr("data-pageid") || -1;

        var selectedNode = $('#tree1').dynatree('getTree').getActiveNode();
        var selectedNodeParent = selectedNode.getParent();
        var selectedNodeParentPageId;

        if (selectedNodeParent.getLevel() == 0) {
            selectedNodeParentPageId = -1;
        }
        else {
            selectedNodeParentPageId = selectedNodeParent.data.pageId;
        }

        function submitPage () {
            var str = $("#addForm").serialize();

            str = str + "&submit=Submit&pageId="+selectedItemId+"&predicateId="+predicateId+"&parentId="+selectedNodeParentPageId;
            $.ajax({
                type: "POST",
                url: "/mainpage/addpage",
                data: str,
                dataType: "json",
                error: ajaxError,
                success: function (ans) {
                    if (ans[0] == "ok") {
                        var pageId = ans[1];
                        $('#tree1').dynatree("getTree").reload();
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

    function changePredicateSelectHandler (n) {
        var url1 = window.location.search;
        var oldPredicate = $('#treeContainer'+n).attr("data-predicateid");
        var newPredicate = $('#predicateSelect'+n).val();
        var mode = $('input[name=predicateRadio'+n+']:checked').val();

        if (mode === 'custom' && oldPredicate != newPredicate) {
            var url2 = setUrlParameter(url1, "CustomPredicate"+n, newPredicate);
            var url3 = removeUrlParameter(url2, "Path"+n);
            window.location.search = url3;
        }
    };

    function changePredicateRadioHandler(n, v) {
        var newPredicate = $('#predicateSelect'+n).val();
        var url1 = window.location.search;
        var url2 = setUrlParameter(url1, "DisplayMode" + n, v);
        var url3 = (v === 'custom') ?
                setUrlParameter(url2, "CustomPredicate"+n, newPredicate) :
                removeUrlParameter(url2, "CustomPredicate" + n);
        window.location.search = url3;
    }

    function URLToArray(url) {
      var request = {};
      var pairs = url.substring(url.indexOf('?') + 1).split('&');
      for (var i = 0; i < pairs.length; i++) {
        var pair = pairs[i].split('=');
        if(pair[0] !== "")
            request[decodeURIComponent(pair[0])] = decodeURIComponent(pair[1]);
      }
      return request;
    }

    function ArrayToURL(array) {
      var pairs = [];
      for (var key in array)
        if (array.hasOwnProperty(key))
          pairs.push(encodeURIComponent(key) + '=' + encodeURIComponent(array[key]));
      return pairs.join('&');
    }

    function setUrlParameter(url, parameter, value) {
        var params = URLToArray(url);
        params[parameter] = value;
        return ArrayToURL(params);
    }

    function removeUrlParameter(url, parameter) {
        var params = URLToArray(url);
        delete params[parameter];
        return ArrayToURL(params);
    }

    function getUrlParameter(url, parameter) {
        var params = URLToArray(url);
        return params[parameter];
    }



    //^^^^^^^^^^^^^ Only function definitions ^^^^^^^^^^^^^^^^^^^^^

    var url1 = window.location.search;
    
    var newUrl1 = setUrlParameter(url1, "test1","test2");

    console.log(newUrl1);


    var treeState = 0;
    //$("div.Content").click(clickTreeItem);

    var loadedPredicate1Id = $('#treeContainer1').attr("data-predicateid");
    var loadedPredicate2Id = $('#treeContainer2').attr("data-predicateid");

    $('#predicateSelect1').change(function() { changePredicateSelectHandler('1'); });
    $('#predicateSelect2').change(function() { changePredicateSelectHandler('2'); });

    $(".predicateRadio1").bind("click", function () {
        changePredicateRadioHandler("1", $(this).attr("value"));
    });

    $(".predicateRadio2").bind("click", function () {
        changePredicateRadioHandler("2", $(this).attr("value"));
    });

    $('#tree1').dynatree({
        onClick: function(node) {
            this.reactivate();
        },
        onActivate: function(node) {
            if(node.tree.isUserEvent()){
                var path = getPathForNode(node);
                var url1 = window.location.search;
                var url2 = setUrlParameter(url1, "Path1", path);
                var url3 = setUrlParameter(url2, "CustomPredicate1", loadedPredicate1Id);
                var url4 = setUrlParameter(url3, "ActiveTree", 1);
                $.cookie('active_tree', 1, { expires: 30 });
                window.location.search = url4;
            }
            else {
                var req = URLToArray(window.location.search);
                var ltree = req.ActiveTree || $.cookie('active_tree');
                if(ltree === '1') displaySelectedPage(node.data.pageId);
            }
        },
        initAjax: {
            url: "/mainpage/tree",
            data: {
                predicateId: loadedPredicate1Id,
                displayMode: $('input[name=predicateRadio1]:checked').val()
            }
        },
        cookieId: "117_tree1_" + loadedPredicate1Id,
        onPostInit: function(isReloading, isError) {
            if(!isReloading) {
                this.visit(function(n) {
                    n.expand(true);
                });
            }

            // 1. Get path

            var path = $('#treeContainer1').attr("data-selectedpath");

            // if path is empty, look it up in cookies

            if (typeof path === "undefined" || path == "") {
                path = $.cookie('path_for_'+loadedPredicate1Id);

                var url1 = window.location.search;
                var url2 = setUrlParameter(url1, "Path1", path);
                var url3 = setUrlParameter(url2, "CustomPredicate1", loadedPredicate1Id);
                window.location.search = url3;
            }
            else {
                var node = getNodeForPath(this.getRoot(), path);

                // 3. TODO: Try to find first real node if necessary

                // 4. Activate node

                var activeNode = this.getActiveNode();

                if (node && (activeNode == null || activeNode.data.key != node.data.key)) {
                    this.activateKey(node.data.key);
                }
                else {
                    this.reactivate();
                }

                // 5. Set cookie

                $.cookie('path_for_'+loadedPredicate1Id, path, { expires: 30 });
            }
        },
        persist: true,
        dnd: {
            onDragStart: function (node) {
                logMsg("tree.onDragStart(%o)", node);
                return true;
            },
            onDrop: function(node, sourceNode, hitMode, ui, draggable) {
                //logMsg("tree.onDrop(%o, %o, %s)", node, sourceNode, hitMode);
                console.log("onDrop:");
                console.log(node);
                console.log(sourceNode);
                console.log(ui);
                console.log(draggable);
                //sourceNode.move(node,hitMode);
            },
            onDragEnter: function (targetNode, sourceNode, ui, draggable) {
                return true;
            }
        }
    });

    $('#tree2').dynatree({
        onClick: function(node) {
            this.reactivate();
        },
        onActivate: function(node) {
            if(node.tree.isUserEvent()){
                var path = getPathForNode(node);
                var url1 = window.location.search;
                var url2 = setUrlParameter(url1, "Path2", path);
                var url3 = setUrlParameter(url2, "CustomPredicate2", loadedPredicate2Id);
                var url4 = setUrlParameter(url3, "ActiveTree", 2);
                $.cookie('active_tree', 2, { expires: 30 });
                window.location.search = url4;
            }
            else {
                var req = URLToArray(window.location.search);
                var ltree = req.ActiveTree || $.cookie('active_tree');
                if(ltree === '2') displaySelectedPage(node.data.pageId);
            }
        },
        initAjax: {
            url: "/mainpage/tree",
            data: {
                predicateId: loadedPredicate2Id,
                displayMode: $('input[name=predicateRadio2]:checked').val()
            }
        },
        cookieId: "117_tree2_" + loadedPredicate2Id,
        onPostInit: function(isReloading, isError) {
            if(!isReloading) {
                this.visit(function(n) {
                    n.expand(true);
                });
            }

            // 1. Get path

            var path = $('#treeContainer2').attr("data-selectedpath");

            // if path is empty, look it up in cookies

            if (typeof path === "undefined" || path == "") {
                path = $.cookie('path_for_'+loadedPredicate2Id);

                var url1 = window.location.search;
                var url2 = setUrlParameter(url1, "Path2", path);
                var url3 = setUrlParameter(url2, "CustomPredicate2", loadedPredicate2Id);
                window.location.search = url3;
            }
            else {
                var node = getNodeForPath(this.getRoot(), path);

                // 3. TODO: Try to find first real node if necessary

                // 4. Activate node

                var activeNode = this.getActiveNode();

                if (node && (activeNode == null || activeNode.data.key != node.data.key)) {
                    this.activateKey(node.data.key);
                }
                else {
                    this.reactivate();
                }

                // 5. Set cookie

                $.cookie('path_for_'+loadedPredicate2Id, path, { expires: 30 });
            }
        },
        persist: true,
        dnd: {
            onDragStart: function (node) {
                logMsg("tree.onDragStart(%o)", node);
                return true;
            },
            onDrop: function(node, sourceNode, hitMode, ui, draggable) {
                //logMsg("tree.onDrop(%o, %o, %s)", node, sourceNode, hitMode);

                console.log("onDrop:");
                console.log(node);
                console.log(sourceNode);
                console.log(ui);
                console.log(draggable);
                //sourceNode.move(node,hitMode);
            },
            onDragEnter: function (targetNode, sourceNode, ui, draggable) {
                return true;
            }
        }
    });


});
