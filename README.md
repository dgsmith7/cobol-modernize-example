# COBOL Banking System Modernization Project

This project demonstrates how a legacy COBOL banking system can be modernized for the web era. First the original COBOL banking system was written, followed by the API, and finally the front end. We wrap the original COBOL core with a Node.js REST API and a modern React web interface, enabling new integrations and user experiences while preserving reliability and data integrity. The concept of the project was to makee all aspeects of the core COBOL system available by way of the web without changing the original COBOL core.

This project was built on a 2023 Macbook Pro M3 with a Pro chip, using gnuCOBOL, NodeJS, Express, and REACT. Storage is accomplished with local files and no DB is used. The project was built as a way or me to deepen my knowledge of COBOL and COBOL modernization techniques through hands-on application using modern tools.

I used github Copilot as an AI assistant during all steps of this project.

I encourage feedback and discussion. Feel free to submit an issue.

## Project Overview

This repository contains a complete, working example of enterprise legacy modernization:

- **COBOL Core**: The original banking logic, file formats, and CLI interface remain unchanged. All business logic, data validation, and transaction processing are handled by the original COBOL programs. Data is stored in sequential files for maximum compatibility.
- **Node.js API Server**: A RESTful API layer that invokes COBOL programs, parses their output, and exposes modern endpoints. The Node.js server exposes REST endpoints for all banking operations. Each API call spawns a COBOL process, passes parameters, and parses the output into JSON. Handles input validation, error translation, and potentially CORS for web access, although it has been tested on a local server.
- **React Frontend**: A responsive, user-friendly web UI for all banking operations, communicating with the API server. The web UI provides account management, transactions, and history views. It uses React Router for navigation and Bootstrap for styling. All operations are performed via API calls—no direct access to COBOL or data files.

## Project Structure

```
cobol-modernize-example/
├── README.md             # This file (project overview)
├── cobol-banking/        # COBOL core system (CLI, copybooks, data files)
│   └── README-COBOL.md         # COBOL system documentation
├── api-server/           # Node.js/Express REST API server
│   └── README-API.md         # API documentation
├── cobol-banking-ui/     # React + Vite web frontend
│   └── README-UI.md         # Frontend documentation
```

## Getting Started

### 1. COBOL Core

See `cobol-banking/README-COBOL.md` for full instructions.

```bash
cd cobol-banking
chmod +x compile.sh demo.sh
./compile.sh
./demo.sh   # Optional: run CLI demo
```

At this point direct commands to the system can be run from the bash terminal (see ./cobol-banking/README-COBOL.md), or the API and front end can be started for a more user-friendly experience.

### 2. API Server

See `api-server/README-API.md` for details.

```bash
cd api-server
npm install
npm start
```

Runs on http://localhost:3001

### 3. React Frontend

See `cobol-banking-ui/README.md` for details.

```bash
cd cobol-banking-ui
npm install
npm run dev
```

Runs on http://localhost:5173

## Integration Flow

```
[User Browser]
      │
      ▼
[React Frontend] ←→ [Node.js API Server] ←→ [COBOL CLI Programs] ←→ [Data Files]
```

## Modernization Highlights

- **No COBOL changes required**: All enhancements are additive and non-invasive. COBOL system can run from the original CLI interface or though the modern interface.
- **Full feature parity**: Every COBOL command is available via API and UI.
- **Robust error handling**: API and UI translate COBOL errors into user-friendly messages.
- **Client-side validation**: The UI prevents most invalid requests before they reach the backend.
- **Consistent data integrity**: All business rules enforced by COBOL remain in effect.

## Extending the Project

- Add new API endpoints by mapping additional COBOL commands.
- Enhance the frontend with charts, reports, or mobile support.
- Replace file storage with a database (optional, requires COBOL changes).
- Integrate authentication or role-based access in the API/UI layers.
- Update CORS for deployment to web.
- Improve data files.
- Continue further testing.
- Improve error handling.

## Documentation

- **COBOL Core**: `cobol-banking/README.md`
- **API Server**: `api-server/README.md`
- **React Frontend**: `cobol-banking-ui/README.md`

## License

This project is provided as-is for educational and demonstration purposes. The repo holds an MIT license. See individual READMEs for more details.
