"use client";

import { Component, ReactNode } from "react";
import { SearchEmpty } from "@shared/components/molecules/empty/search";

type Props = {
  children: ReactNode;
};

type State = {
  hasError: boolean;
};

export class SearchErrorBoundary extends Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = { hasError: false };
  }

  static getDerivedStateFromError(): State {
    return { hasError: true };
  }

  handleRetry = () => {
    this.setState({ hasError: false });
  };

  render() {
    if (this.state.hasError) {
      return <SearchEmpty variant="error" onRetry={this.handleRetry} />;
    }

    return this.props.children;
  }
}
