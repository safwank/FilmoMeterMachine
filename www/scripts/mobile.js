$(document).ready(function() {
	/* Variables */

	var viewModel = {};
	var keyword = $('.keyword');
	var result = $('.result');
	var noResult = $('.no-result');
	var errorResult = $('.error-result');
	var poster = $('.poster');
	var individualResultsShown = false;
	var bindingsApplied = false;

	/* Functions */

	search = function() {
		if (keyword.val() != '') {
			showSpinner();
			resetView();

			$.getJSON(getSearchURI())
			.success(function(data) {
				bindToViewModel(data);
			})
			.error(function() {
				errorResult.slideDown();
			})
			.complete(function() {
				hideSpinner();
			});
		}
	};

	getSearchURI = function() {
		var criteria = keyword.val().split(',');
		var query;

		if (criteria.length === 2) {
			query = $.param({title: criteria[0].trim(), year: criteria[1].trim()});
		}
		else {
			query = $.param({title: criteria[0].trim()});
		}

		return '/search?'.concat(query);
	},

	showSpinner = function() {
		$.mobile.showPageLoadingMsg();
	};

	hideSpinner = function() {
		$.mobile.hidePageLoadingMsg();
	};

	resetView = function() {
		result.slideUp();
		noResult.slideUp();
		errorResult.slideUp();
		resetMeter();
		poster.attr('src', '');
	};

	bindToViewModel = function(data) {
		if (data == null || data === "") {
			noResult.slideDown();
		} else {
			result.slideDown();
					
			if (!bindingsApplied) {
				viewModel = ko.mapping.fromJS(data);
				viewModel.selected = ko.observable();
				viewModel.changeSelected = function(movie) {
					viewModel.selected(movie);
					return true;
				};
				ko.applyBindings(viewModel);
				bindingsApplied = true;
			} else {
				ko.mapping.fromJS(data, viewModel);
			}
		}
	};

	resetIndividualResults = function() {
		individualResults.slideUp();
		individualResultsShown = false;
	};

	searchIfEnterIsPressed = function(e) {
		if (e.which == 13) {
			search();
			keyword.blur();
		}
	};

	resetMeter = function() {
		chart.series[0].points[0].update(0);
	};

	updateMeter = function(value) {
		if (!viewModel.selected) return;

		var point = chart.series[0].points[0];
		point.update(
			viewModel.selected().averageRating(),
			true, 
			{duration: 800});
	};

	/* Events */
	
	keyword.keypress(searchIfEnterIsPressed);
	
	$(document).live('pagechange', function() {
		updateMeter();
	});

	var chart = new Highcharts.Chart({
        chart: {
            renderTo: 'gauge',
            type: 'gauge',
            plotBackgroundColor: null,
            plotBackgroundImage: null,
            plotBorderWidth: 0,
            plotShadow: false,
            backgroundColor: 'transparent'
        },
        
        title: {
            text: ''
        },
        
        pane: {
            startAngle: -150,
            endAngle: 150,
            background: [{
                backgroundColor: {
                    linearGradient: { x1: 0, y1: 0, x2: 0, y2: 1 },
                    stops: [
                        [0, '#FFF'],
                        [1, '#333']
                    ]
                },
                borderWidth: 0,
                outerRadius: '109%'
            }, {
                backgroundColor: {
                    linearGradient: { x1: 0, y1: 0, x2: 0, y2: 1 },
                    stops: [
                        [0, '#333'],
                        [1, '#FFF']
                    ]
                },
                borderWidth: 1,
                outerRadius: '107%'
            }, {
                // default background
            }, {
                backgroundColor: '#DDD',
                borderWidth: 0,
                outerRadius: '105%',
                innerRadius: '103%'
            }]
        },
           
        // the value axis
        yAxis: {
            min: 0,
            max: 10,
            
            minorTickInterval: 'auto',
            minorTickWidth: 1,
            minorTickLength: 10,
            minorTickPosition: 'inside',
            minorTickColor: '#666',
    
            tickPixelInterval: 30,
            tickWidth: 2,
            tickPosition: 'inside',
            tickLength: 10,
            tickColor: '#666',
            labels: {
                step: 2,
                rotation: 'auto'
            },
            title: {
                text: ''
            },
            plotBands: [
	            {
	                from: 8,
	                to: 10,
	                color: '#55BF3B' // green
	            }, 
	            {
	            	from: 7,
	            	to: 8,
	            	color: '#99CC32' // yellowgreen
	            },
	            {
	                from: 4,
	                to: 7,
	                color: '#DDDF0D' // yellow
	            },
	            {
	                from: 2,
	                to: 4,
	                color: '#F87531' // orange
	            },
	            {
	                from: 0,
	                to: 2,
	                color: '#CD0000' // red
	            }
	        ]        
        },
    
        series: [{
            name: 'Rating',
            data: [0],
            tooltip: {
                valueSuffix: '/10'
            }
        }],

        credits: {
        	enabled: false
        }
    }); 
});