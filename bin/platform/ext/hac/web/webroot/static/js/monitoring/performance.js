var buttonTextRunLinpack = 'Run Linpack';
var buttonTextRunSql = 'Run SQL';
var buttonTextRunSqlMax = 'Run SQL Max';
var buttonTextRunOverall = 'Run';
$(document).ready(function() {
    var token = $("meta[name='_csrf']").attr("content");

    $("#tabs").tabs({
		activate : function(event, ui) {
			hac.global.toggleActiveSidebar(ui.newPanel.attr('id').replace(/^.*-/, ''));
		}
	
	});

	$('#runLinpack').click(function(e) {
		hac.global.notify("Running test...");
		$('#resultLinpack').fadeOut();
		$('#runLinpack').text(buttonTextRunLinpack + ' ').append(hac.global.getSpinnerImg());
		$('#runLinpack').attr('disabled', true);
		var url = $('#runLinpack').attr('data-url');
		$.ajax({
			url : url,
			type : 'POST',
			headers : {
				'Accept' : 'application/json',
                'X-CSRF-TOKEN' : token
			},
			success : function(data) {
				debug.log(data);
	
				$('#resultLinpack').text(data.result);
				$('#runLinpack').text(buttonTextRunLinpack);
				$('#resultLinpack').fadeIn();
				$('#runLinpack').removeAttr('disabled');
			},
			error : hac.global.err
		});
	});

	$('#resultSQL').hide();
	
	$('#runSQL').click(function(e) {
		$('#runSQLForm').validate({
			rules: {
				sql: "required",
				seconds: {
					required: true,
					number: true
				},
				count: {
					required: true,
					number: true
				}
			},
			submitHandler: function(form) {
				hac.global.notify("Running test...");

				$('#runSQL').text(buttonTextRunSql + ' ').append(hac.global.getSpinnerImg());
				$('#runSQL').attr('disabled', true);
				var url = $('#runSQL').attr('data-url');
				$.ajax({
					url : url,
					type : 'POST',
					data : 'count=' + $('#count').val() + '&seconds=' + $('#seconds').val() + '&sql=' + encodeURIComponent($('#sql').val()),
					headers : {
						'Accept' : 'application/json',
                        'X-CSRF-TOKEN' : token
					},
					success : function(data) {
						debug.log(data);
						
						if (!data.error) {
							$('#statementsCount').text(data.statementsCount);
							$('#statementsPerSecond').text(data.statementsPerSecond);
							$('#timePerStatement').text(data.timePerStatement);
							$('#resultSQL').fadeIn();
						} else {
							hac.global.error(data.error);
						}
						$('#runSQL').text(buttonTextRunSql);
						$('#runSQL').removeAttr('disabled');
					},
					error : hac.global.err
				});				
			}
		});
		$('#runSQLForm').submit(function(){
			return false;
		});
	});

	$('#runSQLMax').click(function(e) {
		hac.global.notify("Running test...");
		$('#resultMax').fadeOut();
		$('#runSQLMax').text(buttonTextRunSqlMax + ' ').append(hac.global.getSpinnerImg());
		$('#runSQLMax').attr('disabled', true);
		var url = $('#runSQLMax').attr('data-url');
		$.ajax({
			url : url,
			type : 'POST',
			headers : {
				'Accept' : 'application/json',
                'X-CSRF-TOKEN' : token
			},
			success : function(data) {
				debug.log(data);
				
				$('#durationAdded').text(data.durationAdded);
				$('#durationAddedMax').text(data.durationAddedMax);
				$('#durationAddedMaxIndex').text(data.durationAddedMaxIndex);
				$('#runSQLMax').text(buttonTextRunSqlMax);
				$('#resultMax').fadeIn();
				$('#runSQLMax').removeAttr('disabled');
			},
			error : hac.global.err
		});
	});

	$('#runOverall').click(function(e) {
		$('#overallForm').validate({
			rules: {
				secondsPerLoop: {
					required: true,
					number: true
				}
			},
			submitHandler: function(form) {
				hac.global.notify("Running test...");
				$('#resultOverall').fadeOut();
				$('#runOverall').text(buttonTextRunOverall + ' ').append(hac.global.getSpinnerImg());
				$('#runOverall').attr('disabled', true);
				var url = $('#runOverall').attr('data-url');
				$.ajax({
					url : url,
					type : 'POST',
					data : 'seconds=' + $('#secondsPerLoop').val(),
					headers : {
						'Accept' : 'application/json',
                        'X-CSRF-TOKEN' : token
					},
					success : function(data) {
						debug.log(data);

						var resultTable = $('#resultOverallTable').dataTable();
						var dataResults = [];
						$.each(data, function(index,result) {
							dataResults.push([result.name, result.count, result.perSecond ]);
						});
						resultTable.fnClearTable();
						resultTable.fnAddData(dataResults);

						$('#runOverall').text(buttonTextRunOverall);
						$('#resultOverall').fadeIn();
						$('#runOverall').removeAttr('disabled');
					},
					error : hac.global.err
				});				
			}
		});
		$('#overallForm').submit(function(){
			return false;
		});		
	});
});
