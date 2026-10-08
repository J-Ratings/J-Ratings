import { FileBlob, SpreadsheetFile } from "@oai/artifact-tool";

const inputPath = process.argv[2];
if (!inputPath) throw new Error("Workbook path required");

const input = await FileBlob.load(inputPath);
const workbook = await SpreadsheetFile.importXlsx(input);
const payload = [];
for (let i = 0; i < workbook.worksheets.items.length; i += 1) {
  const sheet = workbook.worksheets.getItemAt(i);
  const used = sheet.getUsedRange(true);
  if (!used) continue;
  const rows = used.values ?? [];
  const headers = rows[0] ?? [];
  const columns = headers.map((header, columnIndex) => ({
    header,
    nonBlank: rows.slice(1).filter(row => row[columnIndex] !== null && row[columnIndex] !== "").length,
    distinctSample: [...new Set(rows.slice(1).map(row => row[columnIndex]).filter(value => value !== null && value !== ""))].slice(0, 20),
  }));
  payload.push({
    sheet: sheet.name,
    rowCount: rows.length,
    colCount: rows.reduce((m, row) => Math.max(m, row.length), 0),
    headers,
    columns,
    firstRows: rows.slice(1, 9),
  });
}
console.log(JSON.stringify(payload, null, 2));
