import React, { useEffect } from "react";
import useApi from "../hooks/useApi";
import Spinner from "./Spinner";

export default function AccountList({ onSelect }) {
  const { loading, error, data: accounts, request } = useApi();

  useEffect(() => {
    request("http://localhost:3001/api/accounts");
  }, [request]);

  if (loading) return <Spinner message="Loading accounts..." />;
  if (error) return <div className="alert alert-danger">{error}</div>;
  if (!accounts || !accounts.length)
    return <div className="alert alert-warning">No accounts found.</div>;

  return (
    <div className="card p-3 mb-4" style={{ maxWidth: 600 }}>
      <h2 className="mb-3">Accounts</h2>
      <table className="table table-striped">
        <thead>
          <tr>
            <th>Account Number</th>
            <th>Customer Name</th>
            <th>Balance</th>
            <th>Status</th>
            <th></th>
          </tr>
        </thead>
        <tbody>
          {accounts.map((acc) => (
            <tr key={acc.accountNumber}>
              <td>{acc.accountNumber}</td>
              <td>{acc.customerName}</td>
              <td>${acc.balance}</td>
              <td>{acc.status}</td>
              <td>
                <button
                  className="btn btn-sm btn-outline-primary"
                  onClick={() => onSelect(acc.accountNumber)}
                >
                  View
                </button>
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
