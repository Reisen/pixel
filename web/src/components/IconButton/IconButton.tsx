import React  from 'react';
import styles from './IconButton.module.css';

interface Props {
    icon: string;
}

const IconButton = (props: Props) => (
    <div className={styles.IconButton}>
        {props.icon}
    </div>
);

export default IconButton;
