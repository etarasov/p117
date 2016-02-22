$(document).ready(function () {

    function rebuttonButtons () {
        $("#editButton").button().click(editButtonHandler);
        $("#addButton").button().click(addButtonHandler);
        $("#delButton").button().click(delButtonHandler);
        $("#addPredButton").button().click(addPredButtonHandler);
    };

    var ajaxError = function (x,t,m) {
        alert('Server connection error: '+ t + m);
    };

    function displayPage(pageId) {
        return (pageId == null) ? displayEmptyPage() : displaySelectedPage(pageId);
    }

    function displayEmptyPage () {
        $('#pageText').html('<div id="buttonBar">' +
                            '<button id="addButton">Add</button>'+
                            '<button id="addPredButton">Add Predicate</button>'+
                            '</div>');
        rebuttonButtons();
    }

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
        var req = URLToArray(window.location.search);
        var ltree = req.ActiveTree || '1';
        var predicateId = $('#treeContainer'+ltree).attr("data-predicateId");
        var displayMode = $('input[name=predicateRadio'+ltree+']:checked').val();
        // Get id of page parent in the tree. It's used when create new page at the same level as selected page.
        // TODO: it's a code for previous js tree widget
        //var parentSelectedItemId = $($($($('div.Content[data-pageid="'+selectedItemId+'"]').parent()).parent()).prev()).attr("data-pageid") || -1;

        var selectedNode = $('#tree'+ltree).dynatree('getTree').getActiveNode();

        var selectedItemId = -1, selectedNodeParentPageId = -1;
        if(selectedNode) {
            selectedItemId = selectedNode.data.pageId;
            var selectedNodeParent = selectedNode.getParent();
            if (selectedNodeParent.getLevel() > 0) {
                selectedNodeParentPageId = selectedNodeParent.data.pageId;
            }
        }

        function submitPage () {
            var str = $("#addForm").serialize();

            str = str + "&submit=Submit&pageId="+selectedItemId+"&predicateId="+predicateId+"&parentId="+selectedNodeParentPageId+"&displayMode="+displayMode;
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
                        $('#tree2').dynatree("getTree").reload();
                    }
                    else
                    {
                        alert('Ошибка добавления страницы:'+ans);
                    }
                }
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

    function delButtonHandler () {
        var req = URLToArray(window.location.search);
        var ltree = req.ActiveTree || '1';
        //var predicateId = $('#treeContainer'+ltree).attr("data-predicateId");
        var displayMode = $('input[name=predicateRadio'+ltree+']:checked').val();

        var selectedNode = $('#tree'+ltree).dynatree('getTree').getActiveNode();
        var selectedItemId = selectedNode.data.pageId;
        var selectedNodeParent = selectedNode.getParent();
        var selectedNodeParentPageId;

        if (selectedNodeParent.getLevel() == 0) {
            selectedNodeParentPageId = -1;
        }
        else {
            selectedNodeParentPageId = selectedNodeParent.data.pageId;
        }

        function submitAddPred () {
            var mode = $('#delForm > *[name="mode"]').val();
            var str = "&submit=Submit&pageId="+selectedItemId+"&parentId="+selectedNodeParentPageId+"&mode="+mode;
            $.ajax({
                type: "POST",
                url: "/mainpage/deletepage",
                data: str,
                dataType: "json",
                error: ajaxError,
                success: function (ans) {
                    if (ans == "ok") {
                        $('#tree1').dynatree("getTree").reload();
                        $('#tree2').dynatree("getTree").reload();
                    }
                    else {
                        alert(ans);
                    }
                }
            });

        };

        
        $('#pageText').html('<button id="cancelButton">Cancel</button><button id="submitButton">Submit</button><br>' +
                            '<form id="delForm" method="post">' +
                            '<select name="mode">'+
                            (displayMode === 'custom' ? '<option value="rm_link">Remove link</option>' : '')+
                            '<option value="rm_all_entries">Remove all entries</option>'+
                            '</select><br></form>');
        $('#cancelButton').button().click(function() {
            displaySelectedPage(selectedItemId);
        });
        $('#submitButton').button().click(submitAddPred);
    }

    function addPredButtonHandler () {
        var req = URLToArray(window.location.search);
        var ltree = req.ActiveTree || '1';

        var selectedNode = $('#tree'+ltree).dynatree('getTree').getActiveNode();
        var selectedItemId = selectedNode ? selectedNode.data.pageId : null;

        function submitAddPred () {
            var title = $('#addPredForm > *[name="title"]').val();
            var str = "submit=Submit&title="+ encodeURIComponent(title);
            $.ajax({
                type: "POST",
                url: "/mainpage/addpredicatepage",
                data: str,
                dataType: "json",
                error: ajaxError,
                success: function (ans) {
                    if (ans[0] === "ok") {
                        var url1 = window.location.search;
                        var url2 = setUrlParameter(url1, "CustomPredicate"+ltree, ans[1]);
                        var url3 = removeUrlParameter(url2, "Path"+ltree);
                        var url4 = setUrlParameter(url3, "DisplayMode"+ltree, "custom");
                        window.location.search = url4;
                    }
                    else {
                        alert(ans);
                    }
                }
            });

        };

        $('#pageText').html('<button id="cancelButton">Cancel</button><button id="submitButton">Submit</button><br>' +
                            '<form id="addPredForm" method="post">' +
                            '<br>Name <input type="text" size="32" maxlength="40" name="title" value="">' +
                            '<br></form>');
        $('#cancelButton').button().click(function() {
            displayPage(selectedItemId);
        });
        $('#submitButton').button().click(submitAddPred);
        $('#addPredForm').submit(submitAddPred);
    }

    function changePredicateSelectHandler (n) {
        var url1 = window.location.search;
        var oldPredicate = $('#treeContainer'+n).attr("data-predicateid");
        var newPredicate = $('#predicateSelect'+n).val();
        var mode = $('input[name=predicateRadio'+n+']:checked').val();

        if (mode === 'custom' && oldPredicate != newPredicate) {
            var url2 = setUrlParameter(url1, "CustomPredicate"+n, newPredicate);
            var url3 = setUrlParameter(url2, "ActiveTree", n);
            var url4 = removeUrlParameter(url3, "Path"+n);
            window.location.search = url4;
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

    function onDragOverHandler (targetNode, sourceNode, hitMode, ui, draggable) {
        if(hitMode !== 'over') {
            targetNode = targetNode.getParent();
        }
        var srcPageId = sourceNode.data.pageId;
        if(srcPageId === targetNode.data.pageId) return false;
        if(targetNode.isDescendantOf(sourceNode)) return false;
        var children = targetNode.getChildren();
        if(children !== null) {
            for(var i = 0; i < children.length; i++) {
                if(children[i].data.pageId === srcPageId) return false;
            }
        }
        return true;
    }

    function onDropHandler(predicateId, targetNode, sourceNode, hitMode) {
        var targetPageId = targetNode.data.pageId;
        if(hitMode !== 'over') targetPageId = targetNode.getParent().data.pageId;
        if(targetPageId === undefined) targetPageId = -1;
        $.ajax({
            type: "POST",
            url: "/mainpage/copypage",
            data: "srcPageId="+sourceNode.data.pageId
                +"&predicateId="+predicateId
                +"&targetPageId="+targetPageId,
            dataType: "json",
            error: ajaxError,
            success: function (ans) {
                if(ans && ans[0] === 'error') {
                    alert("Error: " + ans[1]);
                }
                else {
                    sourceNode.tree.reload();
                    targetNode.tree.reload();
                }
            }
        });
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

            // Get url params

            var req = URLToArray(window.location.search);
            var ltree = req.ActiveTree || $.cookie('active_tree');

            // if path is empty, look it up in cookies

            if (typeof path === "undefined" || path == "") {
                path = $.cookie('path_for_'+loadedPredicate1Id);
                req["Path1"] = path;
                req["CustomPredicate1"] = loadedPredicate1Id;
                window.location.search = ArrayToURL(req);
            }
            else {
                var node = getNodeForPath(this.getRoot(), path);

                // 3. TODO: Try to find first real node if necessary

                // 4. Activate node

                var activeNode = this.getActiveNode();

                if (node && (activeNode == null || activeNode.data.key != node.data.key)) {
                    this.activateKey(node.data.key);
                }
                else if (activeNode) {
                    this.reactivate();
                }
                else {
                    if(ltree === '1') displayPage();
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
            onDrop: function(targetNode, sourceNode, hitMode, ui, draggable) {
                onDropHandler(loadedPredicate1Id, targetNode, sourceNode, hitMode);
            },
            onDragOver: onDragOverHandler,
            onDragEnter: function (targetNode, sourceNode, ui, draggable) {
                var displayMode = $('input[name=predicateRadio1]:checked').val();
                return (displayMode === 'allpages') ? false : true;
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

            // Get url params

            var req = URLToArray(window.location.search);
            var ltree = req.ActiveTree || $.cookie('active_tree');

            // if path is empty, look it up in cookies

            if (typeof path === "undefined" || path == "") {
                path = $.cookie('path_for_'+loadedPredicate2Id);
                req["Path2"] = path;
                req["CustomPredicate2"] = loadedPredicate2Id;
                window.location.search = ArrayToURL(req);
            }
            else {
                var node = getNodeForPath(this.getRoot(), path);

                // 3. TODO: Try to find first real node if necessary

                // 4. Activate node

                var activeNode = this.getActiveNode();

                if (node && (activeNode == null || activeNode.data.key != node.data.key)) {
                    this.activateKey(node.data.key);
                }
                else if (activeNode) {
                    this.reactivate();
                }
                else {
                    if(ltree === '2') displayPage();
                }

                // 5. Set cookie

                $.cookie('path_for_'+loadedPredicate2Id, path, { expires: 30 });
            }
        },
        persist: true,
        dnd: {
            onDragStart: function (node) {
                return true;
            },
            onDrop: function(targetNode, sourceNode, hitMode, ui, draggable) {
                onDropHandler(loadedPredicate2Id, targetNode, sourceNode, hitMode);
            },
            onDragOver: onDragOverHandler,
            onDragEnter: function (targetNode, sourceNode, ui, draggable) {
                var displayMode = $('input[name=predicateRadio2]:checked').val();
                return (displayMode === 'allpages') ? false : true;
            }
        }
    });


});
