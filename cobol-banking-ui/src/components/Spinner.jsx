import React from "react";

export default function Spinner({ message = "Loading..." }) {
  return (
    <div className="d-flex flex-column align-items-center my-4">
      <div
        className="spinner-border text-primary mb-2"
        role="status"
        style={{ width: "3rem", height: "3rem" }}
      >
        <span className="visually-hidden">{message}</span>
      </div>
      <div>{message}</div>
    </div>
  );
}
