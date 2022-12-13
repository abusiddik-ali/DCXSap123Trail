var tablesizes, analyzeTable, logEnabled, tracesEnabled, paramsEnabled, logUpdateIntervalId, sizeSlider, currentDownloadSize = -1;
var buttonTextAnalyze = "Analyze";
var buttonTextDownloadLog = "Download log";
var buttonTableSizes = "Calculate Table Sizes"

$(document).ready(function() {
	$( "#tabs" ).tabs({
		activate: function(event, ui) { 
			if ( ui.newPanel.attr('id') == 'tabs-3') {
				refreshLoggingInfo();
			}
			
			toggleActiveSidebar(ui.newPanel.attr('id').replace(/^.*-/, ''));
		}
	});
	
	$('#buttonTableSizes').click(reloadTableSizes);
	
	// tab 1
	tablesizes = $('#tablesizes').dataTable({
		"bStateSave": true,
		"bAutoWidth": false,
		"aLengthMenu" : [[10,25,50,100,-1], [10,25,50,100,'all']]
	});
	
	analyzeTable = $('#analyzeTable').dataTable({
		"bAutoWidth": false,
		"aaSorting": [ [3,'desc']],
	});
	
	sizeSlider = $("#slider-size").slider({
		min : 0,
		max : 100,
		change : function(event, ui) {
			$('#downloadLog').html(buttonTextDownloadLog + " (last " + ui.value + "kB)");
			currentDownloadSize = ui.value;
		}
	});		

	// tab 0
	rebuildDataSourceInfos();
	
	$("body").on('click', '.resetButton',function(e) {
        var token = $("meta[name='_csrf']").attr("content");

        var url = $('#dataSourceInfos').attr('data-url') + this.id;
		$.ajax({
			url:url,
			type:'POST',
			headers:{
                'Accept':'application/json',
                'X-CSRF-TOKEN' : token
            },
			success: function(data) {
				$('#ds_' + data.dsId).fadeOut('fast', function(e) {
					$(this).remove();
				});
				appendDataSourceInfo(data);
			},
			error: hac.global.err
		});					
		
	});
	
	$('#toggleLogging').click(function(e){
        var token = $("meta[name='_csrf']").attr("content");
        var url = $('#toggleLogging').attr('data-url') + !logEnabled;
		$.ajax({
			url:url,
			type:'POST',
			headers:{
                'Accept':'application/json',
                'X-CSRF-TOKEN' : token
            },
			success: function(data) {
				debug.log(data);
				logEnabled = !logEnabled;
				updateLoggingUI();
				if (logEnabled) {
					logUpdateIntervalId = setInterval("refreshLoggingInfo()", 2000);
				}
				else if (logUpdateIntervalId) {
					clearInterval(logUpdateIntervalId);
				}
			},
			error: hac.global.err
		});				
		
	});
	
	$('#toggleTraces').click(function(e){
        var token = $("meta[name='_csrf']").attr("content");
        var url = $('#toggleTraces').attr('data-url') + !tracesEnabled;
		$.ajax({
			url:url,
			type:'POST',
			headers:{
                'Accept':'application/json',
                'X-CSRF-TOKEN' : token
            },
			success: function(data) {
				debug.log(data);
				tracesEnabled = !tracesEnabled;
				updateLoggingUI();
			},
			error: hac.global.err
		});				
	});

	$('#toggleParams').click(function(e){
        var token = $("meta[name='_csrf']").attr("content");
        var url = $('#toggleParams').attr('data-url') + !paramsEnabled;
		$.ajax({
			url:url,
			type:'POST',
			headers:{
                'Accept':'application/json',
                'X-CSRF-TOKEN' : token
            },
			success: function(data) {
				debug.log(data);
                paramsEnabled = !paramsEnabled;
				updateLoggingUI();
			},
			error: hac.global.err
		});
	});

	$('#clearLog').click(function(e){
        var token = $("meta[name='_csrf']").attr("content");
        var url = $('#clearLog').attr('data-url');
		$.ajax({
			url:url,
			type:'POST',
			headers:{
                'Accept':'application/json',
                'X-CSRF-TOKEN' : token
            },
			success: function(deleteResult) {
				debug.log(deleteResult);
				if (deleteResult.success)
				{
					$('#logFileSize').html('0');
					$('#downloadLog').fadeOut();
					$('#slider-size').fadeOut();
				}
				else if(deleteResult.error)
				{
					hac.global.error(data.error);
				}
			},
			error: hac.global.err
		});				
		
	});
	
	$('#downloadLog').click(function(e){
		$('#downloadSize').val(currentDownloadSize);
		$('#downloadForm').submit();
	});		
	
	$('#analyzeLog').click(function(e){
		var url = $('#analyzeLog').attr('data-url');
        var token = $("meta[name='_csrf']").attr("content");

        $('#analyzeLog').html(buttonTextAnalyze + ' ' +  hac.global.getSpinnerImg());
		$.ajax({
			url:url,
			type:'GET',
			headers:{
                'Accept':'application/json',
                'X-CSRF-TOKEN' : token
            },
			success: function(data) {
				debug.log(data);
                if (data.success) {

                    $('#totalQueries').html(data.totalQueries);
                    $('#totalTime').html(data.totalTime);

                    analyzeTable.fnClearTable();
                    var pos;
                    for (pos in data.queryMap) {
                        var obj = data.queryMap[pos];
                        analyzeTable.fnAddData([getTruncatedQuery(pos), obj.count, obj.time, Math.round(obj.time / data.totalTime * 100)]);
                    }

                    $('#analyzeResults').fadeIn();
                    $('#analyzeLog').html(buttonTextAnalyze);
                }
                else{
                    hac.global.error(data.error);
                }
			},
			error: hac.global.err
		});				
	});
});	


function getTruncatedQuery(query)
{
    var html = $("<div/>").append($("<span>", {title: query}).text(query.substring(0, 200))).html();
	debug.log(html);
	return html;
}

function toggleActiveSidebar(num)
{
	// fadeout
	$("div[id^=sidebar]").each(function() { 
		$(this).fadeOut();
	});
	setTimeout("$('#sidebar"+num+"').fadeIn();", 500);
}

function refreshLoggingInfo()
{
    var token = $("meta[name='_csrf']").attr("content");
    var url = $('#loggingContentWrapper').attr('data-refreshLoggingInfoUrl');
	$.ajax({
		url:url,
		type:'GET',
		headers:{
            'Accept':'application/json',
            'X-CSRF-TOKEN' : token
        },
		success: function(data) {
			debug.log(data);
			
			$('#loggingSpinnerWrapper').fadeOut('fast', function(e){
				logEnabled = data.logEnabled;
				tracesEnabled = data.tracesEnabled;
				
				updateLoggingUI();
				
				$('#logFilePath').html(data.logFilePath);
				$('#logFileSize').html(data.logFileSize);
				$('#slider-size').slider( "option", "max", data.logFileSize );

				if (parseInt(data.logFileSize, 10) > 0)
				{
					$('#downloadLog').fadeIn();
					$('#slider-size').fadeIn();
				}
				
				if (logEnabled && !logUpdateIntervalId) {
					logUpdateIntervalId = setInterval("refreshLoggingInfo()", 2000);
				}
				
				$('#loggingContentWrapper').fadeIn();
			});
		},
		error: hac.global.err
	});		
}

function updateLoggingUI()
{
	$('#toggleLogging').html(logEnabled ? 'Stop Logging ' + hac.global.getSpinnerImg() : "Start logging");
	$('#toggleTraces').html(tracesEnabled ? "Disable Traces" : "Enable traces");
	$('#toggleParams').html(paramsEnabled ? "Disable Params" : "Enable Params");
}


function reloadTableSizes()
{
	$('#tableWrapper').fadeOut();
	tablesizes.fnClearTable();
	
	$('#buttonTableSizes').html(buttonTableSizes + ' ' +  hac.global.getSpinnerImg());
    var token = $("meta[name='_csrf']").attr("content");

    var url = $('#tablesizes').attr('data-reloadTableSizesUrl');
	$.ajax({
		url:url,
		type:'GET',
		headers:{
            'Accept':'application/json',
            'X-CSRF-TOKEN' : token
        },
		success: function(data) {
			debug.log(data);
		
			$('#buttonTableSizes').html(buttonTableSizes);
			
			for (pos in data)
			{
				tablesizes.fnAddData([pos, data[pos]]);
			}
			
			$("#tableWrapper").fadeIn();
			
		},
		error: hac.global.err
	});				
}

function rebuildDataSourceInfos()
{
	// clear all
	$('#dataSourceInfos').html('');
    var token = $("meta[name='_csrf']").attr("content");
    var url = $('#dataSourceInfos').attr('data-rebuildDataSourceInfosUrl');
	$.ajax({
		url:url,
		type:'GET',
		headers:{
            'Accept':'application/json',
            'X-CSRF-TOKEN' : token
        },
		success: function(data) {
			for (pos in data) {
				appendDataSourceInfo(data[pos]);
			}
		},
		error: hac.global.err
	});			
}

function appendDataSourceInfo(datasource)
{
    let legend = $('<legend>').text(datasource.dsId + (datasource.active ? ' - active' : ''));

    let button = $('<button>', {
        style: 'float:right;',
        class: 'resetButton',
        id: datasource.dsId
    }).text("Reset statistics");

    let connInfo = connectionInfo(datasource);
    let dl = $('<dl>');

	if (datasource.jndi)
	{
        dl.append(dlEntryTextareaStyle(dl, "jndiName"));
	}
	else
	{
        let databaseProps = ["url", "dbName", "dbUser", "dbVersion", "dbDriverVersion"];

        if (datasource.tablePrefix)
        {
			databaseProps[databaseProps.length] = "tablePrefixName";
		}
		for (pos in databaseProps)
		{
            let value = datasource[databaseProps[pos]];
            if (value != null && value.length > 100)
            {
                dl.append(dlEntryTextareaStyle(databaseProps[pos], value));
			}
            else
            {
                dl.append(dlEntryStyle(databaseProps[pos], value));
		}
		}
	}

    let fieldset = $('<fieldset>', {id: `ds_${datasource.dsId}`});
    fieldset.append(legend);
    fieldset.append(button);
    fieldset.append(connInfo);
    fieldset.append(dl);
    fieldset.append($('<hr />'));

	$('#dataSourceInfos').append(fieldset);
}

function connectionInfo(datasource)
{
	var dsInUse = (datasource.numInUse === 1) ? " data source" : " data sources";
	var dsOpen = (datasource.numPhysicalOpen === 1) ? " data source" : " data sources";
	var dsRoOpen = (datasource.numReadOnlyOpen === 1) ? " data source" : " data sources";
	var dsRoOpenVerb = (datasource.numReadOnlyOpen === 1) ? " is" : " are";

    return $('<p>').append('This data source currently has ')
        .append($('<strong>').text(datasource.numInUse + dsInUse))
        .append(' in use and ')
        .append($('<strong>').text(datasource.numPhysicalOpen + dsOpen))
        .append(' opened (of which ')
        .append($('<strong>').text(datasource.numReadOnlyOpen + dsRoOpen))
        .append(dsRoOpenVerb)
        .append(' read-only). The maximum number of simultaneously opened connections is ')
        .append($('<strong>').text(datasource.maxPhysicalOpen))
        .append('. According to the configuration, the connection pool can have a maximum of ')
        .append($('<strong>').text(datasource.maxAllowedOpen))
        .append(' connections open. The total number of getConnection() calls is ')
        .append($('<strong>').text(datasource.totalGets))
        .append(' and the average time to get a connection is ')
        .append($('<strong>').text(datasource.millisWaitedForConn))
        .append(' ms.');
}

function dlEntryStyle(key, value)
{
    return [
        $('<dt>').text(key),
        $('<dd>').text(value)
    ];
}

function dlEntryTextareaStyle(key, value)
{
    return [
        $('<dt>').text(key),
        $('<dd>').append($('<textarea>', {
            class: "nobox textarea",
            style: "min-height:50px; background:white;"
        }).val(value))
    ];
}
