const fs = require('fs');
const express = require('express')
const app = express()

function filterSudokuFiles (fileName) {
	return !fileName.startsWith('.');
}

function randomIndex(length) {
	return Math.floor(Math.random() * length)  
}

// Add headers
app.use(function (req, res, next) {

    // Website you wish to allow to connect
    res.setHeader('Access-Control-Allow-Origin', 'null');

    // Request methods you wish to allow
    res.setHeader('Access-Control-Allow-Methods', 'GET, POST');

    // Request headers you wish to allow
    res.setHeader('Access-Control-Allow-Headers', 'X-Requested-With,content-type');

    // Pass to next layer of middleware
    next();
});

app.get('/sudoku/new', function (req, res) {
	fs.readdir('data/', 'utf8', (err, data) => {
		if (err) throw err;
		sudokuFiles = data.filter(filterSudokuFiles);
		selectedFile = sudokuFiles[randomIndex(sudokuFiles.length)];
		fs.readFile('data/' + selectedFile, 'utf8', (err, data) => {
			if (err) throw err;
			res.send(JSON.stringify(convertDataToBoard(data)));	
		})
	});
})


app.get('/sudoku/:id', function (req, res) {
	var id = req.params.id
	fs.readFile('data/' + id + '.txt', 'utf8', (err, data) => {
		if (err) throw err;
		res.send(JSON.stringify(convertDataToBoard(data)));
	});
})

function convertDataToBoard(data) {
	var toReturn = [];
	var rows = data.split('\n'); 
	for (var rowIndex = 0; rowIndex < rows.length; rowIndex++) {
		var columns = rows[rowIndex].split('');
		for (var columnIndex = 0; columnIndex < columns.length; columnIndex++) {
			toReturn.push(createBox(columnIndex + 1, rowIndex + 1, parseInt(columns[columnIndex])));
		}
	}
	
	return toReturn;
}

function createBox(column, row, value) {
	return {value: value, row: row, column: column, locked: true};
}


app.listen(3000, function () {
  console.log('Example app listening on port 3000!');
})
