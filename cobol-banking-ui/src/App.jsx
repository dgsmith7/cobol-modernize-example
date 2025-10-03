import React from "react";
import { Routes, Route, useNavigate, useParams } from "react-router-dom";
import AccountList from "./components/AccountList.jsx";
import AccountDetail from "./components/AccountDetail.jsx";
import AccountForm from "./components/AccountForm.jsx";

function App() {
  const navigate = useNavigate();

  return (
    <div className="bg-light min-vh-100 d-flex justify-content-center align-items-center">
      <div
        className="d-flex flex-column align-items-center w-100"
        style={{ maxWidth: 800 }}
      >
        <h1 className="mb-4 text-center">COBOL Banking System</h1>
        <Routes>
          <Route
            path="/"
            element={
              <>
                <AccountForm onSuccess={() => window.location.reload()} />
                <AccountList
                  onSelect={(accountNumber) =>
                    navigate(`/accounts/${accountNumber}`)
                  }
                />
              </>
            }
          />
          <Route
            path="/accounts/:accountNumber"
            element={<AccountDetailWrapper />}
          />
        </Routes>
      </div>
    </div>
  );
}

// Wrapper to extract accountNumber param and pass to AccountDetail
function AccountDetailWrapper() {
  const { accountNumber } = useParams();
  const navigate = useNavigate();
  return (
    <AccountDetail accountNumber={accountNumber} onBack={() => navigate("/")} />
  );
}

export default App;
