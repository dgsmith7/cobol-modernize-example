import React, { useEffect } from "react";
import useApi from "../hooks/useApi";
import DepositWithdrawForm from "./DepositWithdrawForm";
import TransferForm from "./TransferForm";
import Spinner from "./Spinner";

export default function AccountDetail({ accountNumber, onBack }) {
  const {
    loading: loadingAccount,
    error: errorAccount,
    data: account,
    request: fetchAccount,
  } = useApi();

  const {
    loading: loadingHistory,
    error: errorHistory,
    data: history,
    request: fetchHistory,
  } = useApi();

  // Fetch account details and history on mount or when accountNumber changes
  useEffect(() => {
    fetchAccount(`http://localhost:3001/api/accounts/${accountNumber}`);
    fetchHistory(`http://localhost:3001/api/accounts/${accountNumber}/history`);
    // eslint-disable-next-line
  }, [accountNumber]);

  // Helper to reload both account and history after a transaction
  const reload = () => {
    fetchAccount(`http://localhost:3001/api/accounts/${accountNumber}`);
    fetchHistory(`http://localhost:3001/api/accounts/${accountNumber}/history`);
  };

  if (loadingAccount || loadingHistory)
    return <Spinner message="Loading account details..." />;
  if (errorAccount)
    return <div className="alert alert-danger">{errorAccount}</div>;
  if (!account)
    return <div className="alert alert-danger">Account not found.</div>;

  return (
    <div>
      <button className="btn btn-link mb-3" onClick={onBack}>
        &larr; Back to Accounts
      </button>
      <div className="card p-3 mb-4" style={{ maxWidth: 500 }}>
        <h2 className="mb-3">Account Details</h2>
        <p>
          <b>Account Number:</b> {account.accountNumber}
        </p>
        <p>
          <b>Customer Name:</b> {account.customerName}
        </p>
        <p>
          <b>Balance:</b> ${account.balance}
        </p>
        <p>
          <b>Status:</b> {account.status}
        </p>
      </div>
      <DepositWithdrawForm
        accountNumber={account.accountNumber}
        onSuccess={reload}
      />
      <TransferForm fromAccount={account.accountNumber} onSuccess={reload} />
      <TransactionHistory
        history={Array.isArray(history) ? history : []}
        loading={loadingHistory}
        error={errorHistory}
      />
    </div>
  );
}

function TransactionHistory({ history, loading, error }) {
  if (loading) return <Spinner message="Loading transaction history..." />;
  if (error) return <div className="alert alert-danger">{error}</div>;

  return (
    <div className="card p-3 mb-4" style={{ maxWidth: 700 }}>
      <h3 className="mb-3">Transaction History</h3>
      {history.length === 0 ? (
        <p>No transactions found.</p>
      ) : (
        <div className="table-responsive">
          <table className="table table-striped table-bordered">
            <thead>
              <tr>
                <th>Date</th>
                <th>Time</th>
                <th>Type</th>
                <th>Amount</th>
                <th>Description</th>
                <th>Status</th>
              </tr>
            </thead>
            <tbody>
              {history.map((txn, idx) => (
                <tr key={idx}>
                  <td>{txn.date}</td>
                  <td>{txn.time}</td>
                  <td>{txn.type}</td>
                  <td>${txn.amount}</td>
                  <td>{txn.description}</td>
                  <td>{txn.status}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
    </div>
  );
}
