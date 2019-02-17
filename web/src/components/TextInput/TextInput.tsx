import React  from 'react';
import styles from './TextInput.module.css';

interface Props {
    icon?:        string;
    placeholder: string;
}

const TextInput = (props: Props) => (
    <input
        className={styles.TextInput}
        placeholder={props.placeholder}
    />
);

export default TextInput;
