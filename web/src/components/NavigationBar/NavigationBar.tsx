import React    from 'react';
import styles   from './NavigationBar.module.css';
import { Link } from 'react-router-dom';

const NavigationBar = () => (
    <div className={styles.NavigationBar}>
        <Link to="/">
            PIXEL
        </Link>
    </div>
);

export default NavigationBar;
