$(function() {

    var generateScriptsExecuteURL = $('#generateScriptsForm').attr('data-url');
    var previewScriptsExecuteURL = $('#previewScriptsForm').attr('data-url');
    initUI();

    function initUI() {
        $('.buttonExecute').attr('disabled', false);
    }

    var executeScriptGeneration = function (url, postData) {
        var token = $("meta[name='_csrf']").attr("content");

        $.ajax({
            url: url,
            type: 'POST',
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json; charset=utf-8',
                'X-CSRF-TOKEN' : token
            },
            data: JSON.stringify(postData),
            success: function (data) {
                if (data.success) {
                    $("#generatingScripts").hide();
                    $('#file').val('');
                    $('#previewDDLFile').html(data.ddlFileName);
                    $('#previewDMLFile').html(data.dmlFileName);
                    $("#generatedScriptsArea").show("slow", function () {
                        // Animation complete.
                    });
                    if (data.initialize) {
                        $('#previewDropDDLFileDiv').show();
                        $('#previewDropDDLFile').html(data.ddlDropFileName);
                    } else {
                        $('#previewDropDDLFileDiv').hide();
                    }
                } else {
                    hac.global.error("Trouble... check logs!");
                }
                $('.buttonExecute').attr('disabled', false);
            },
            error: hac.global.err
        });
    };

    $('.buttonExecute').click(function (e) {
        $('.buttonExecute').attr('disabled', true);
        $("#generatingScripts").show();
        $("#generatedScriptsArea").hide("slow", function () {
            //
        });
        $("#textarea-container").hide();
        var postData = {};
        postData.initialize = $('#initialize').get(0).checked;
        postData.update = $('#update').get(0).checked;
        executeScriptGeneration(generateScriptsExecuteURL, postData);
    });

    var loading = function () {
        $("#textarea-container").show();
        $('#file').val($('#generatingScripts').html());
    };

    var executeScriptPreview = function (url, postData) {
        var token = $("meta[name='_csrf']").attr("content");

        $.ajax({
            url: url,
            type: 'POST',
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json; charset=utf-8',
                'X-CSRF-TOKEN' : token
            },
            data: JSON.stringify(postData),
            success: function (data) {
                if (data.success) {
                    $('#file').val(data.preview);
                } else {
                    hac.global.error("Trouble... check logs!");
                }
            },
            error: hac.global.err
        });
    };

    $('.preview').click(function (e) {
        loading();
        var postData = {};
        postData.button = $(this).attr('data-id');
        executeScriptPreview(previewScriptsExecuteURL, postData);
    });
});